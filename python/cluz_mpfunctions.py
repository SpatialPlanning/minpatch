"""
/***************************************************************************
                                 A QGIS plugin
 CLUZ for QGIS
                             -------------------
        begin                : 2024-07-29
        copyright            : (C) 2024 by Bob Smith, DICE
        email                : r.j.smith@kent.ac.uk
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""

from copy import deepcopy
from csv import reader

from .cluz_messages import clear_progress_bar, make_progress_bar, set_progress_bar_value


def create_mp_running_unit_dictionary(minpatch_data_dict, marxan_sol_location_string):
    pre_marxan_unit_dict = minpatch_data_dict['initial_unit_dict']
    init_unit_dict = deepcopy(pre_marxan_unit_dict)
    a_marxan_sol_dict = make_mp_marxan_sol_dict(marxan_sol_location_string)
    running_unit_dict = make_mp_start_unit_dict(init_unit_dict, a_marxan_sol_dict)

    return running_unit_dict


def make_mp_marxan_sol_dict(marxan_sol_location_string):
    marxan_sol_dict = dict()
    with open(marxan_sol_location_string, 'rt') as f:
        sol_reader = reader(f)
        next(sol_reader)
        for aRow in sol_reader:
            pu_id = int(aRow[0])
            sol_value = int(aRow[1])
            marxan_sol_dict[pu_id] = sol_value

    return marxan_sol_dict


def make_mp_start_unit_dict(pu_dict, marxan_sol_dict):
    for aRow in marxan_sol_dict:
        sol_pu_status = marxan_sol_dict[aRow]
        if sol_pu_status == 1:
            pu_list = pu_dict[aRow]
            pu_list[1] = 1
            pu_dict[aRow] = pu_list

    return pu_dict


def pu_status_does_not_equal_excluded(pu_dict, pu_id):
    not_excluded_bool = True
    if pu_dict[pu_id][1] == 3:
        not_excluded_bool = False

    return not_excluded_bool


def pu_status_is_earmarked_or_conserved(pu_dict, pu_id):
    is_earmarked_or_conserved = False
    if pu_dict[pu_id][1] == 1 or pu_dict[pu_id][1] == 2:
        is_earmarked_or_conserved = True

    return is_earmarked_or_conserved


def make_mp_patch_dict(pu_dict, minpatch_data_dict):
    area_dict = minpatch_data_dict['area_dict']
    boundary_matrix_dict = minpatch_data_dict['boundary_matrix_dict']
    patch_dict = dict()
    patch_id = 1
    running_portfolio_pu_id_set = make_portfolio_pu_id_set(pu_dict)  # To contain data on all PUs in portfolio, then each PU will be removed & assiged to a patch

    while len(running_portfolio_pu_id_set) > 0:
        patch_pu_id_set = make_patch_pu_id_set(boundary_matrix_dict, running_portfolio_pu_id_set)
        running_portfolio_pu_id_set = running_portfolio_pu_id_set.difference(patch_pu_id_set)
        patch_pu_id_list = list(patch_pu_id_set)
        patch_pu_id_list.sort()
        patch_area = return_patch_area(area_dict, patch_pu_id_list)
        patch_dict[patch_id] = [patch_area, len(patch_pu_id_list), patch_pu_id_list]
        patch_id += 1

    return patch_dict


def make_portfolio_pu_id_set(unit_dict):
    portfolio_pu_id_set = set()
    for pu_id in unit_dict:
        if pu_status_is_earmarked_or_conserved(unit_dict, pu_id):
            portfolio_pu_id_set.add(pu_id)

    return portfolio_pu_id_set


def make_patch_pu_id_set(boundary_matrix_dict, running_portfolio_pu_id_set):
    loop_unit_set = set()
    patch_pu_id_set = set()
    initial_pu_id = list(running_portfolio_pu_id_set)[0]
    loop_unit_set.add(initial_pu_id)

    while len(loop_unit_set) > 0:
        pu_id = loop_unit_set.pop()
        patch_pu_id_set.add(pu_id)
        neighb_list = boundary_matrix_dict[pu_id]
        if len(neighb_list) > 0:
            for neighb_pu_id in neighb_list:
                if neighb_pu_id in running_portfolio_pu_id_set and neighb_pu_id not in patch_pu_id_set:
                    loop_unit_set.add(neighb_pu_id)

    return patch_pu_id_set


def return_patch_area(area_dict, patch_pu_id_list):
    patch_area = 0
    for pu_id in patch_pu_id_list:
        patch_area += area_dict[pu_id]

    return patch_area


def rem_small_patches_from_unit_dict(minpatch_data_dict, unit_dict, patch_dict, marxan_file_name):
    pre_marxan_unit_dict = minpatch_data_dict['initial_unit_dict']
    zone_dict = minpatch_data_dict['zone_dict']

    progress_bar = make_progress_bar('Removing small patches:' + marxan_file_name)
    row_total_count = len(patch_dict)
    row_count = 1

    for patchID in patch_dict:
        set_progress_bar_value(progress_bar, row_count, row_total_count)
        row_count += 1
        patch_size = patch_dict[patchID][0]
        patch_size_threshold = calc_patch_size_threshold(zone_dict, patch_dict, patchID)

        if patch_size < patch_size_threshold:
            patch_id_list = patch_dict[patchID][2]
            for unitIDValue in patch_id_list:
                orig_status = pre_marxan_unit_dict[unitIDValue][1]
                unit_list = unit_dict[unitIDValue]
                unit_status = unit_list[1]
                if orig_status == 0 and unit_status == 1:
                    unit_list[1] = 0
                    unit_dict[unitIDValue] = unit_list

    clear_progress_bar()
    return unit_dict


def calc_patch_size_threshold(zone_dict, patch_dict, patch_id):
    patch_pu_id_list = patch_dict[patch_id][2]
    if len(zone_dict) == 1:
        first_pu_id = patch_pu_id_list[0]  # Takes patch threshold size from first PU, as every PU has same threshold
        patch_size_threshold = zone_dict[first_pu_id][1]
    else:
        patch_pu_id_list = patch_dict[patch_id][2]
        patch_size_threshold = 'blank'
        for pu_id in patch_pu_id_list:
            pu_patch_threshold = zone_dict[pu_id][1]
            if patch_size_threshold == 'blank':
                patch_size_threshold = pu_patch_threshold
            if patch_size_threshold < pu_patch_threshold:
                patch_size_threshold = pu_patch_threshold

    return patch_size_threshold


def add_mp_patches(setup_object, minpatch_data_dict, running_unit_dict, marxan_file_name):
    continue_bool = True
    feat_amount_cons_dict = make_mp_feat_amount_cons_dict(setup_object, minpatch_data_dict, running_unit_dict)
    unmet_target_id_set = make_mp_unmet_target_id_set(feat_amount_cons_dict, minpatch_data_dict)
    pu_selection_set = make_mp_pu_selection_set(minpatch_data_dict, running_unit_dict)
    pu_patch_set_dict = make_mp_pu_patch_set_dict(pu_selection_set, minpatch_data_dict)
    all_pu_patch_abund_dict = make_mp_pu_patch_abund_dict(minpatch_data_dict, running_unit_dict, pu_selection_set, pu_patch_set_dict, unmet_target_id_set)

    progress_bar = make_progress_bar('Adding new patches:' + marxan_file_name)
    row_total_count = len(unmet_target_id_set)

    while len(unmet_target_id_set) > 0:
        row_count = row_total_count - len(unmet_target_id_set)
        set_progress_bar_value(progress_bar, row_count, row_total_count)

        pu_patch_score_dict = make_pu_patch_score_dict(minpatch_data_dict, feat_amount_cons_dict, all_pu_patch_abund_dict, pu_selection_set)
        pu_id, pu_selection_set = return_best_pu(pu_patch_score_dict)

        if pu_id == -1:
            continue_bool = False
            break

        running_unit_dict = add_patch(minpatch_data_dict, running_unit_dict, pu_id)
        pu_selection_set.remove(pu_id)

        all_pu_patch_abund_dict = update_pu_patch_abund_dict(all_pu_patch_abund_dict, minpatch_data_dict, running_unit_dict, pu_selection_set, pu_patch_set_dict, unmet_target_id_set, pu_id)
        feat_amount_cons_dict = make_mp_feat_amount_cons_dict(setup_object, minpatch_data_dict, running_unit_dict)
        unmet_target_id_set = make_mp_unmet_target_id_set(feat_amount_cons_dict, minpatch_data_dict)
    clear_progress_bar()

    return running_unit_dict, feat_amount_cons_dict, unmet_target_id_set, continue_bool


def make_feature_id_list_of_unmeetable_targets_string(unmet_target_id_set):
    feat_id_list_string = ''
    for featID in unmet_target_id_set:
        feat_id_list_string = feat_id_list_string + str(featID) + ', '

    final_feat_id_list_string = feat_id_list_string[0:-2]

    return final_feat_id_list_string


def make_mp_feat_amount_cons_dict(setup_object, minpatch_data_dict, unit_dict):
    target_dict = minpatch_data_dict['target_dict']
    abund_matrix_dict = minpatch_data_dict['abund_matrix_dict']

    feat_amount_cons_dict = dict(zip(target_dict.keys(), len(target_dict.keys()) * [0]))

    for pu_id in abund_matrix_dict:
        pu_status = unit_dict[pu_id][1]
        if pu_status == 1 or pu_status == 2:
            pu_abund_dict = abund_matrix_dict[pu_id]
            for featID in pu_abund_dict:
                feat_amount = pu_abund_dict[featID]
                con_total_amount = feat_amount_cons_dict[featID]
                con_total_amount += feat_amount
                final_con_total_amount = round(con_total_amount, setup_object.decimal_places)
                feat_amount_cons_dict[featID] = final_con_total_amount

    return feat_amount_cons_dict


def make_mp_unmet_target_id_set(amount_con_dict, minpatch_data_dict):
    unmet_target_set = set()
    target_dict = minpatch_data_dict['target_dict']
    for featID in amount_con_dict:
        amount_conserved = amount_con_dict[featID]
        target_value = target_dict[featID][1]

        if target_value > 0 and amount_conserved < target_value:
            unmet_target_set.add(featID)

    return unmet_target_set


def make_mp_pu_selection_set(minpatch_data_dict, running_unit_dict):
    pu_selection_set = set()
    add_patch_pu_id_dict = minpatch_data_dict['add_patch_pu_id_dict']
    for pu_id in running_unit_dict:
        if pu_status_does_not_equal_excluded(running_unit_dict, pu_id):
            neighb_patch_id_list = add_patch_pu_id_dict[pu_id]
            if len(neighb_patch_id_list) > 0:
                pu_selection_set.add(pu_id)

    return pu_selection_set


def make_mp_pu_patch_set_dict(pu_selection_set, minpatch_data_dict):
    pu_patch_set_dict = dict()
    add_patch_pu_id_dict = minpatch_data_dict['add_patch_pu_id_dict']
    for pu_id in pu_selection_set:
        pu_set = set(add_patch_pu_id_dict[pu_id])
        pu_set.add(pu_id)
        pu_patch_set_dict[pu_id] = pu_set

    return pu_patch_set_dict


def make_mp_pu_patch_abund_dict(minpatch_data_dict, unit_dict, pu_selection_set, pu_patch_set_dict, unmet_target_id_set):
    all_pu_patch_abund_dict = dict()
    for pu_id in pu_selection_set:
        patch_cost = return_single_pu_patch_cost(unit_dict, pu_patch_set_dict, pu_id)
        pu_patch_abund_dict = make_single_pu_patch_abund_dict(minpatch_data_dict, unit_dict, pu_patch_set_dict, unmet_target_id_set, pu_id)
        all_pu_patch_abund_dict[pu_id] = [pu_patch_abund_dict, patch_cost]

    return all_pu_patch_abund_dict


def return_single_pu_patch_cost(unit_dict, pu_patch_set_dict, pu_id):
    patch_pu_id_set = pu_patch_set_dict[pu_id]
    patch_cost = 0

    for pu_patch_id in patch_pu_id_set:
        pu_status = unit_dict[pu_patch_id][1]
        pu_cost = unit_dict[pu_patch_id][0]
        if pu_status == 0:
            patch_cost += pu_cost

    return patch_cost


def make_single_pu_patch_abund_dict(minpatch_data_dict, unit_dict, pu_patch_set_dict, unmet_target_id_set, pu_id):
    abund_matrix_dict = minpatch_data_dict['abund_matrix_dict']
    patch_pu_id_set = pu_patch_set_dict[pu_id]
    pu_patch_abund_dict = dict()

    for featID in unmet_target_id_set:
        patch_amount = 0
        for puPatchID in patch_pu_id_set:
            pu_patch_status = unit_dict[puPatchID][1]
            if pu_patch_status == 0:
                abund_dict = abund_matrix_dict[puPatchID]
                try:
                    abund_amount = abund_dict[featID]
                except KeyError:
                    abund_amount = 0
                patch_amount += abund_amount
        if patch_amount > 0:
            pu_patch_abund_dict[featID] = patch_amount

    return pu_patch_abund_dict


def make_pu_patch_score_dict(minpatch_data_dict, feat_amount_cons_dict, all_pu_patch_abund_dict, pu_selection_set):
    pu_patch_score_dict = dict()
    for pu_id in pu_selection_set:
        pu_patch_score_dict[pu_id] = calc_pu_patch_score(minpatch_data_dict, feat_amount_cons_dict, all_pu_patch_abund_dict, pu_id)

    return pu_patch_score_dict


def calc_pu_patch_score(minpatch_data_dict, feat_amount_cons_dict, all_pu_patch_abund_dict, pu_id):
    target_dict = minpatch_data_dict['target_dict']
    pu_score = 0
    pu_patch_abund_dict = all_pu_patch_abund_dict[pu_id][0]
    pu_patch_cost = all_pu_patch_abund_dict[pu_id][1]
    for featID in pu_patch_abund_dict:
        feat_score = calc_pu_patch_feature_score(target_dict, feat_amount_cons_dict, pu_patch_abund_dict, featID)
        if feat_score != 'blank':
            pu_score += feat_score

    try:
        final_pu_score = pu_score / pu_patch_cost
    except ArithmeticError:
        final_pu_score = 0

    return final_pu_score


def calc_pu_patch_feature_score(target_dict, feat_amount_cons_dict, pu_patch_abund_dict, feat_id):
    feat_score = 'blank'
    patch_amount = pu_patch_abund_dict[feat_id]
    target_amount = target_dict[feat_id][1]
    con_amount = feat_amount_cons_dict[feat_id]
    target_gap = target_amount - con_amount
    if target_gap > 0:
        feat_score = patch_amount / target_gap

        if feat_score > 1:  # Reduce feat_score if over 1, as we only need to meet the target
            feat_score = 1

    return feat_score


def return_best_pu(pu_patch_score_dict):
    pu_selection_set = set()
    pu_id_value = -1
    running_score = 0
    for pu_id in pu_patch_score_dict:
        score_value = pu_patch_score_dict[pu_id]
        if score_value > 0:
            pu_selection_set.add(pu_id)
        # If joint equal then always selects first PU in list
        if score_value > running_score:
            running_score = score_value
            pu_id_value = pu_id

    return pu_id_value, pu_selection_set


def add_patch(minpatch_data_dict, unit_dict, pu_id_value):
    add_patch_pu_id_dict = minpatch_data_dict['add_patch_pu_id_dict']
    init_pu_list = add_patch_pu_id_dict[pu_id_value]
    pu_list = init_pu_list + [pu_id_value]
    for a_pu_id_value in pu_list:
        pu_list = unit_dict[a_pu_id_value]
        pu_status = pu_list[1]
        if pu_status == 0:
            pu_list[1] = 1
            unit_dict[a_pu_id_value] = pu_list
    return unit_dict


def update_pu_patch_abund_dict(all_pu_patch_abund_dict, minpatch_data_dict, unit_dict, pu_selection_list, pu_patch_set_dict, unmet_target_id_list, best_pu_id):
    abund_matrix_dict = minpatch_data_dict['abund_matrix_dict']
    best_patch_set = pu_patch_set_dict[best_pu_id]

    for a_patch_centre_pu in pu_selection_list:
        a_patch_pu_id_set = pu_patch_set_dict[a_patch_centre_pu]
        set_over_lap = best_patch_set.intersection(a_patch_pu_id_set)
        if len(set_over_lap) > 0:
            a_patch_cost = 0
            for b_pu_id in a_patch_pu_id_set:
                b_pu_status = unit_dict[b_pu_id][1]
                b_pu_cost = unit_dict[b_pu_id][0]
                if b_pu_status == 0:
                    a_patch_cost += b_pu_cost

            pu_patch_abund_dict = dict()
            for featID in unmet_target_id_list:
                feat_patch_amount = 0
                for c_pu_id in a_patch_pu_id_set:
                    abund_dict = abund_matrix_dict[c_pu_id]
                    if unit_dict[c_pu_id][1] == 0 and featID in abund_dict:
                        abund_amount = abund_dict[featID]
                        feat_patch_amount += abund_amount
                if feat_patch_amount > 0:
                    pu_patch_abund_dict[featID] = feat_patch_amount

            all_pu_patch_abund_dict[a_patch_centre_pu] = [pu_patch_abund_dict, a_patch_cost]

    return all_pu_patch_abund_dict


def run_sim_whittle(setup_object, running_unit_dict, minpatch_data_dict, marxan_file_name):
    patch_dict = make_mp_patch_dict(running_unit_dict, minpatch_data_dict)
    raw_edge_pu_id_set = make_edge_pu_id_set(running_unit_dict, minpatch_data_dict)
    pu_patch_id_dict = make_pu_patch_id_dict(running_unit_dict, patch_dict)
    feat_amount_cons_dict = make_mp_feat_amount_cons_dict(setup_object, minpatch_data_dict, running_unit_dict)
    keystone_pu_id_set = set()     # Keystone list is of PUs that can't be removed without affecting patch size or targets
    costly_pu_id_set = set()  # List of PUs that increases portfolio so shouldn't be removed. PUs REMOVED WHEN NEIGHBOURING PLANNING UNITS ARE WHITTLED.

    candidate_edge_pu_id_set, keystone_pu_id_set = make_edge_pu_id_sets(minpatch_data_dict, running_unit_dict, patch_dict, pu_patch_id_dict, raw_edge_pu_id_set, keystone_pu_id_set)

    progress_bar = make_progress_bar('Whittling away:' + marxan_file_name)

    whittle_pu_id = 'initialising'
    while whittle_pu_id != 'blank':
        whittle_score_dict, keystone_pu_id_set = make_whittle_score_dict_keystone_set(minpatch_data_dict, feat_amount_cons_dict, candidate_edge_pu_id_set, keystone_pu_id_set)
        whittle_pu_id, keystone_pu_id_set, costly_pu_id_set = return_whittle_pu_id_keystone_set(minpatch_data_dict, running_unit_dict, patch_dict, pu_patch_id_dict, whittle_score_dict, keystone_pu_id_set, costly_pu_id_set)
        if whittle_pu_id != 'blank':
            running_unit_dict = remove_whittle_pu(running_unit_dict, whittle_pu_id)
            feat_amount_cons_dict = make_mp_feat_amount_cons_dict(setup_object, minpatch_data_dict, running_unit_dict)
            patch_dict = make_mp_patch_dict(running_unit_dict, minpatch_data_dict)
            pu_patch_id_dict = make_pu_patch_id_dict(running_unit_dict, patch_dict)

            candidate_edge_pu_id_set.remove(whittle_pu_id)

            costly_pu_id_set = update_costly_pu_id_set_to_remove_neighb_of_whittle_pu_id(minpatch_data_dict, costly_pu_id_set, whittle_pu_id)
            excluded_from_candidate_edge_pu_id_set = keystone_pu_id_set.union(costly_pu_id_set)

            candidate_edge_pu_id_set = candidate_edge_pu_id_set.difference(excluded_from_candidate_edge_pu_id_set)
            neighb_edge_pu_set = make_neighb_edge_pu_set(minpatch_data_dict, running_unit_dict, excluded_from_candidate_edge_pu_id_set, whittle_pu_id)
            candidate_edge_pu_id_set = candidate_edge_pu_id_set.union(neighb_edge_pu_set)

        set_progress_bar_value(progress_bar, len(costly_pu_id_set) + len(keystone_pu_id_set), len(running_unit_dict))

    clear_progress_bar()

    return running_unit_dict


def update_costly_pu_id_set_to_remove_neighb_of_whittle_pu_id(minpatch_data_dict, costly_pu_id_set, pu_id):
    bound_matrix_dict = minpatch_data_dict['boundary_matrix_dict']
    neighb_list = list(bound_matrix_dict[pu_id].keys())
    neighb_set = set(neighb_list)

    updated_costly_pu_id_set = costly_pu_id_set.difference(neighb_set)

    return updated_costly_pu_id_set


def return_whittle_pu_id_keystone_set(mp_data_dict, pu_dict, patch_dict, pu_patch_id_dict, whittle_score_dict, keystone_pu_id_set, costly_pu_id_set):
    bound_matrix_dict = mp_data_dict['boundary_matrix_dict']
    whittle_pu_id = 'blank'
    while whittle_score_dict_not_empty(whittle_score_dict) and whittle_pu_id == 'blank':
        candidate_pu_id = return_candidate_whittle_pu_id(whittle_score_dict)
        if removing_pu_increases_marxan_cost(mp_data_dict, pu_dict, bound_matrix_dict, candidate_pu_id):
            costly_pu_id_set.add(candidate_pu_id)
            whittle_score_dict.pop(candidate_pu_id)
        else:
            if removing_pu_makes_patch_too_small(mp_data_dict, patch_dict, pu_patch_id_dict, candidate_pu_id):
                keystone_pu_id_set.add(candidate_pu_id)
                whittle_score_dict.pop(candidate_pu_id)
            else:
                if removing_pu_splits_into_nonviable_patches(mp_data_dict, pu_dict, patch_dict, pu_patch_id_dict, candidate_pu_id):
                    keystone_pu_id_set.add(candidate_pu_id)
                    whittle_score_dict.pop(candidate_pu_id)
                else:
                    whittle_pu_id = candidate_pu_id

    return whittle_pu_id, keystone_pu_id_set, costly_pu_id_set


def removing_pu_increases_marxan_cost(minpatch_data_dict, unit_dict, bound_matrix_dict, candidate_pu_id):
    marxan_blm = minpatch_data_dict['bound_cost']
    pu_increases_marxan_cost_bool = False

    pu_cost = unit_dict[candidate_pu_id][0]
    neighb_detail_dict = bound_matrix_dict[candidate_pu_id]
    marx_neighb_list = neighb_detail_dict.keys()
    edge_score = 0
    for neighbID in marx_neighb_list:
        neighb_status = unit_dict[neighbID][1]
        if neighb_status == 1 or neighb_status == 2:
            edge_score += neighb_detail_dict[neighbID]
        if neighb_status == 0 or neighb_status == 3:
            edge_score -= neighb_detail_dict[neighbID]

    edge_score *= marxan_blm
    final_edge_score = edge_score  # Removed mention of blmFudgeWeight
    if pu_cost < final_edge_score:
        pu_increases_marxan_cost_bool = True

    return pu_increases_marxan_cost_bool


def removing_pu_makes_patch_too_small(mp_data_dict, patch_dict, pu_patch_id_dict, pu_id):
    area_dict = mp_data_dict['area_dict']
    zone_dict = mp_data_dict['zone_dict']
    removing_pu_makes_patch_too_small_bool = False

    pu_size = area_dict[pu_id]
    patch_id = pu_patch_id_dict[pu_id]
    patch_size = patch_dict[patch_id][0]
    patch_size_threshold = calc_patch_size_threshold(zone_dict, patch_dict, patch_id)
    if patch_size - pu_size < patch_size_threshold:
        removing_pu_makes_patch_too_small_bool = True

    return removing_pu_makes_patch_too_small_bool


def removing_pu_splits_into_nonviable_patches(mp_data_dict, pu_dict, patch_dict, pu_patch_id_dict, candidate_pu_id):
    zone_dict = mp_data_dict['zone_dict']
    removing_pu_splits_into_nonviable_patches_bool = False

    after_split_patch_dict = make_after_split_patch_dict(mp_data_dict, pu_dict, patch_dict, pu_patch_id_dict, candidate_pu_id)

    for patchID in after_split_patch_dict:
        patch_size_threshold = calc_patch_size_threshold(zone_dict, after_split_patch_dict, patchID)
        if after_split_patch_dict[patchID][0] < patch_size_threshold:
            removing_pu_splits_into_nonviable_patches_bool = True

    return removing_pu_splits_into_nonviable_patches_bool


def make_after_split_patch_dict(mp_data_dict, unit_dict, patch_dict, pu_patch_id_dict, candidate_pu_id):
    to_split_patch_pu_dict = dict()
    patch_id = pu_patch_id_dict[candidate_pu_id]
    patch_pu_id_list = patch_dict[patch_id][2]

    for pu_id in patch_pu_id_list:
        to_split_patch_pu_dict[pu_id] = unit_dict[pu_id]
    to_split_patch_pu_dict.pop(candidate_pu_id)

    after_split_patch_dict = make_mp_patch_dict(to_split_patch_pu_dict, mp_data_dict)

    return after_split_patch_dict


def whittle_score_dict_not_empty(whittle_score_dict):
    is_whittle_score_dict_not_empty = True
    if len(whittle_score_dict) == 0:
        is_whittle_score_dict_not_empty = False

    return is_whittle_score_dict_not_empty


def return_candidate_whittle_pu_id(whittle_score_dict):
    candidate_pu_id = 'blank'
    candidate_score = 'blank'
    for pu_id in whittle_score_dict:
        whittle_score = whittle_score_dict[pu_id]
        if candidate_pu_id == 'blank' or whittle_score < candidate_score:
            candidate_score = whittle_score
            candidate_pu_id = pu_id
        if whittle_score < candidate_score:
            candidate_score = whittle_score
            candidate_pu_id = pu_id

    return candidate_pu_id


def make_neighb_edge_pu_set(minpatch_data_dict, unit_dict, keystone_pu_id_set, pu_id):
    bound_matrix_dict = minpatch_data_dict['boundary_matrix_dict']
    neighb_list = bound_matrix_dict[pu_id].keys()
    neighb_edge_set = set()
    for neighb_pu_id in neighb_list:
        neighb_pu_is_on_edge = False
        if neighb_pu_id not in keystone_pu_id_set and unit_dict[neighb_pu_id][1] == 1:
            neighb_neighb_list = bound_matrix_dict[neighb_pu_id].keys()
            for neighb_neighb_pu_id in neighb_neighb_list:
                neighb_neighb_status = unit_dict[neighb_neighb_pu_id][1]
                if neighb_neighb_status == 0 or neighb_neighb_status == 3:
                    neighb_pu_is_on_edge = True
            if neighb_pu_is_on_edge:
                neighb_edge_set.add(neighb_pu_id)

    return neighb_edge_set


def make_edge_pu_id_sets(minpatch_data_dict, pu_dict, patch_dict, pu_patch_id_dict, edge_pu_id_set, keystone_pu_id_set):
    pre_marxan_unit_dict = minpatch_data_dict['initial_unit_dict']
    area_dict = minpatch_data_dict['area_dict']
    zone_dict = minpatch_data_dict['zone_dict']

    viable_edge_pu_id_set = set()
    for edgePU in edge_pu_id_set:
        edge_pu_status = pu_dict[edgePU][1]
        edge_pu_init_status = pre_marxan_unit_dict[edgePU][1]

        if edge_pu_status == 1 and edge_pu_init_status != 2:
            edge_pu_area = area_dict[edgePU]
            edge_pu_patch_id = pu_patch_id_dict[edgePU]
            patch_area = patch_dict[edge_pu_patch_id][0]
            patch_area_with_pu_removed = patch_area - edge_pu_area

            min_patch_size = lookup_min_size_value_of_patch(zone_dict, patch_dict, edge_pu_patch_id)

            if edge_pu_status == 1 and patch_area_with_pu_removed >= min_patch_size:
                viable_edge_pu_id_set.add(edgePU)
            elif edge_pu_status == 1 and patch_area_with_pu_removed < min_patch_size:
                keystone_pu_id_set.add(edgePU)

    return viable_edge_pu_id_set, keystone_pu_id_set


def make_pu_patch_id_dict(unit_dict, patch_dict):
    pu_patch_id_dict = dict(zip(list(unit_dict.keys()), len(unit_dict) * [0]))
    for patchID in patch_dict:
        patch_list = patch_dict[patchID][2]
        for patch_pu_id in patch_list:
            pu_patch_id_dict[patch_pu_id] = patchID

    return pu_patch_id_dict


def make_whittle_score_dict_keystone_set(minpatch_data_dict, feat_amount_cons_dict, edge_pu_id_set, keystone_pu_id_set):
    abund_matrix_dict = minpatch_data_dict['abund_matrix_dict']
    target_dict = minpatch_data_dict['target_dict']
    whittle_score_dict = dict()

    for edge_pu_id in edge_pu_id_set:
        whittle_score = calc_pu_whittle_score(abund_matrix_dict, target_dict, feat_amount_cons_dict, edge_pu_id)
        if whittle_score == 'PU cannot be whittled, as needed to meet targets':
            keystone_pu_id_set.add(edge_pu_id)
        else:
            whittle_score_dict[edge_pu_id] = whittle_score

    return whittle_score_dict, keystone_pu_id_set


def calc_pu_whittle_score(abund_matrix_dict, target_dict, feat_amount_cons_dict, pu_id):
    pu_abund_dict = abund_matrix_dict[pu_id]
    feat_score_list = list()
    for aFeat in pu_abund_dict:
        feat_amount = pu_abund_dict[aFeat]
        feat_target = target_dict[aFeat][1]
        feat_con_amount = feat_amount_cons_dict[aFeat]

        if pu_needed_to_meet_target(feat_target, feat_con_amount, feat_amount):
            feat_score_list.append('Cannot be removed')
        else:
            try:
                whittle_calc = feat_amount / (feat_con_amount - feat_target)
            except ZeroDivisionError:
                whittle_calc = 0
            feat_score_list.append(whittle_calc)

    if 'Cannot be removed' in feat_score_list:
        whittle_score = 'PU cannot be whittled, as needed to meet targets'
    else:
        try:
            whittle_score = max(feat_score_list)
        except ValueError:
            whittle_score = 0

    return whittle_score


def pu_needed_to_meet_target(feat_target, feat_con_amount, feat_amount):
    pu_needed_to_meet_target_bool = False
    feat_con_amount_minus_pu = feat_con_amount - feat_amount

    if feat_con_amount_minus_pu < feat_target and feat_target > 0:
        pu_needed_to_meet_target_bool = True

    return pu_needed_to_meet_target_bool


def make_edge_pu_id_set(unit_dict, minpatch_data_dict):
    edge_pu_id_set = set()
    boundary_matrix_dict = minpatch_data_dict['boundary_matrix_dict']
    for pu_id in unit_dict:
        if pu_is_on_edge(boundary_matrix_dict, unit_dict, pu_id):
            edge_pu_id_set.add(pu_id)

    return edge_pu_id_set


def pu_is_on_edge(boundary_matrix_dict, unit_dict, pu_id):
    edge_bool = False
    pu_status = unit_dict[pu_id][1]
    if pu_status == 1:
        neighb_list = boundary_matrix_dict[pu_id].keys()
        for neighbID in neighb_list:
            neighb_status = unit_dict[neighbID][1]
            # Check if neighbour is available, excluded or if PU has edge with itself (ie on edge of planning region)
            if neighb_status == 0 or neighb_status == 3 or pu_id == neighbID:
                edge_bool = True

    return edge_bool


def lookup_min_size_value_of_patch(zone_dict, patch_dict, edge_pu_patch_id):
    patch_pu_id_list = patch_dict[edge_pu_patch_id][2]
    min_patch_size = -1

    for thePU in patch_pu_id_list:
        zone_min_patch_size = zone_dict[thePU][1]
        if min_patch_size == -1:
            min_patch_size = zone_min_patch_size
        if min_patch_size < zone_min_patch_size:
            min_patch_size = zone_min_patch_size

    return min_patch_size


def remove_whittle_pu(unit_dict, whittle_pu_id):
    pu_list = unit_dict[whittle_pu_id]
    pu_list[1] = 0
    unit_dict[whittle_pu_id] = pu_list

    return unit_dict


def add_conserved_pus(running_unit_dict, minpatch_data_dict):
    init_unit_dict = minpatch_data_dict['initial_unit_dict']
    for puUnitValue in running_unit_dict:
        if init_unit_dict[puUnitValue][1] == 2:
            pu_list = running_unit_dict[puUnitValue]
            pu_list[1] = 2
            running_unit_dict[puUnitValue] = pu_list

    return running_unit_dict


def make_mp_cost_dict(minpatch_data_dict, pu_dict):
    cost_dict = dict()

    abund_matrix_dict = minpatch_data_dict['abund_matrix_dict']
    target_dict = minpatch_data_dict['target_dict']
    boundary_matrix_dict = minpatch_data_dict['boundary_matrix_dict']
    target_list = list(target_dict.keys())
    target_list.sort()

    abund_values_dict, num_active_pus = make_abund_values_dict_num_active_pus(target_list, abund_matrix_dict, pu_dict)
    cost_dict['abundance_values_dict'] = abund_values_dict
    cost_dict['number_active_pus'] = num_active_pus

    total_unit_cost, con_unit_count = calc_unit_costs(pu_dict)
    cost_dict['total_unit_cost'] = total_unit_cost
    cost_dict['con_unit_count'] = con_unit_count

    amount_conserved_dict = make_amount_conserved_dictionary(target_list, abund_matrix_dict, pu_dict)
    cost_dict['amount_conserved_dict'] = amount_conserved_dict
    cost_dict['total_target_cost'] = make_total_target_cost(amount_conserved_dict, target_dict)

    total_bound_length, total_boundary_cost = make_bound_costs(minpatch_data_dict, boundary_matrix_dict, pu_dict)
    cost_dict['total_boundary_length'] = total_bound_length
    cost_dict['total_boundary_cost'] = total_boundary_cost

    return cost_dict


def make_abund_values_dict_num_active_pus(target_list, abund_matrix_dict, pu_dict):
    num_active_pus = 0
    abund_values_dict = dict()
    for aRow in target_list:
        abund_values_dict[aRow] = [0, 0, 0, 0]

    for pu_id in abund_matrix_dict:
        pu_list = pu_dict[pu_id]
        pu_status = pu_list[1]
        # Count the number of units that could be selected in the iteration section
        if pu_status == 0 or pu_status == 1:
            num_active_pus += 1
        pu_abund_dict = abund_matrix_dict[pu_id]
        for aFeature in pu_abund_dict:
            the_amount = pu_abund_dict[aFeature]
            feature_list = abund_values_dict[aFeature]
            running_value = feature_list[pu_status]
            running_value += the_amount
            feature_list[pu_status] = running_value
            abund_values_dict[aFeature] = feature_list

    return abund_values_dict, num_active_pus


def calc_unit_costs(pu_dict):
    total_unit_cost = 0
    con_unit_count = 0
    for pu_id in pu_dict:
        the_list = pu_dict[pu_id]
        unit_value, unit_status = the_list
        if unit_status == 1 or unit_status == 2:
            total_unit_cost += unit_value
            con_unit_count += 1

    return total_unit_cost, con_unit_count


def make_amount_conserved_dictionary(target_list, abund_matrix_dict, unit_dict):
    amount_conserved_dict = dict()

    for bNum in target_list:
        amount_conserved_dict[bNum] = 0

    for pu_id in abund_matrix_dict:
        pu_status = unit_dict[pu_id][1]
        if pu_status == 1 or pu_status == 2:
            pu_abund_dict = abund_matrix_dict[pu_id]
            for featID in pu_abund_dict:
                feat_amount = pu_abund_dict[featID]
                con_total_value = amount_conserved_dict[featID]
                con_total_value += feat_amount
                amount_conserved_dict[featID] = con_total_value

    return amount_conserved_dict


def make_total_target_cost(amount_conserved_dict, target_dict):
    total_target_cost = 0
    for featID in amount_conserved_dict:
        amount_conserved = amount_conserved_dict[featID]
        target_values_list = target_dict[featID]
        the_target = target_values_list[1]
        the_penalty = target_values_list[2]
        if amount_conserved < the_target:
            total_target_cost = total_target_cost + the_penalty

    return total_target_cost


def make_bound_costs(minpatch_data_dict, boundary_matrix_dict, pu_dict):
    total_bound_length = calc_mp_total_bound_length(boundary_matrix_dict, pu_dict)

    blm_value = minpatch_data_dict['bound_cost']
    total_boundary_cost = total_bound_length * blm_value

    return total_bound_length, total_boundary_cost


def calc_mp_total_bound_length(boundary_matrix_dict, pu_dict):
    total_bound_length = 0

    for id1Value in boundary_matrix_dict:
        pu_bound_dict = boundary_matrix_dict[id1Value]
        for id2Value in pu_bound_dict:
            if id2Value >= id1Value:
                bound_value = pu_bound_dict[id2Value]
                con_count = 0
                id1_status_value = pu_dict[id1Value][1]
                id2_status_value = pu_dict[id2Value][1]

                if id1_status_value == 1 or id1_status_value == 2:
                    con_count += 1
                if id2_status_value == 1 or id2_status_value == 2:
                    con_count += 1
                if con_count == 1:
                    total_bound_length += bound_value
                # Allow for external edges
                if con_count == 2 and id1Value == id2Value:
                    total_bound_length += bound_value

    return total_bound_length


def marxan_polish_pu_dict(minpatch_data_dictionary_dict, pu_dict, polish_edge_pu_set):
    bound_matrix_dict = minpatch_data_dictionary_dict['boundary_matrix_dict']
    marxan_blm = minpatch_data_dictionary_dict['bound_cost']
    while len(polish_edge_pu_set) > 0:
        edge_pu = polish_edge_pu_set.pop()
        neighb_list = list(bound_matrix_dict[edge_pu].keys())
        for neighbPU in neighb_list:
            polish_cost = make_polish_cost(pu_dict, bound_matrix_dict, marxan_blm, neighbPU)

            if polish_cost < 0:
                polish_edge_pu_set.add(neighbPU)
                neighb_pu_list = pu_dict[neighbPU]
                neighb_pu_list[1] = 1
                pu_dict[neighbPU] = neighb_pu_list

    return pu_dict


def make_polish_cost(unit_dict, bound_matrix_dict, marxan_blm, neighb_pu_id):
    neighb_status = unit_dict[neighb_pu_id][1]
    if neighb_status == 0:
        neighb_cost = unit_dict[neighb_pu_id][0]
        neigh_neighb_dict = bound_matrix_dict[neighb_pu_id]
        neighb_bound_cost = 0
        for neighNeighbPU in neigh_neighb_dict:
            neigh_neighb_pu_status = unit_dict[neighNeighbPU][1]
            neigh_neighb_bound_length = neigh_neighb_dict[neighNeighbPU]
            if neigh_neighb_pu_status == 1 or neigh_neighb_pu_status == 2:
                neighb_bound_cost -= neigh_neighb_bound_length
            if neigh_neighb_pu_status == 0 or neigh_neighb_pu_status == 3:
                neighb_bound_cost += neigh_neighb_bound_length

        polish_cost = neighb_cost + (neighb_bound_cost * marxan_blm)

    else:
        polish_cost = 'blank'

    return polish_cost
