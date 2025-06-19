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
from csv import writer
from os import path
from statistics import median, StatisticsError

from .cluz_mpfunctions import calc_patch_size_threshold


def make_mp_patch_stats_dict(patch_dict, minpatch_data_dict):
    zone_dict = minpatch_data_dict['zone_dict']
    patch_stats_dict = dict()
    all_area_list, valid_area_list = make_patch_area_lists(patch_dict, zone_dict)

    try:
        median_all_patch = median(all_area_list)
    except StatisticsError:
        median_all_patch = 0

    try:
        median_valid_patch = median(valid_area_list)
    except StatisticsError:
        median_valid_patch = 0

    patch_stats_dict['all_patch_count'] = len(all_area_list)
    patch_stats_dict['all_patch_area'] = sum(all_area_list)
    patch_stats_dict['median_all_patch'] = median_all_patch
    patch_stats_dict['valid_patch_count'] = len(valid_area_list)
    patch_stats_dict['valid_patch_area'] = sum(valid_area_list)
    patch_stats_dict['median_valid_patch'] = median_valid_patch

    return patch_stats_dict


def make_patch_area_lists(patch_dict, zone_dict):
    all_area_list = list()
    valid_area_list = list()
    for patchID in patch_dict:
        patch_area = patch_dict[patchID][0]
        patch_size_threshold = calc_patch_size_threshold(zone_dict, patch_dict, patchID)

        all_area_list.append(patch_area)
        if patch_area >= patch_size_threshold:
            valid_area_list.append(patch_area)

    return all_area_list, valid_area_list


def print_mp_patch_list_dict(patch_neighb_list_dict, zone_type_dict, output_path):
    with open(output_path, 'w', newline='', encoding='utf-8') as outputFile:
        output_writer = writer(outputFile)
        output_writer.writerow(['*** MinPatch v3.0'])
    
        for zoneID in zone_type_dict:
            zone_radius = zone_type_dict[zoneID][1]
            output_writer.writerow(['*** Patch search distance for Zone ' + str(zoneID) + ' = ' + str(zone_radius)])
    
        for pu_id in patch_neighb_list_dict:
            line_value_list = deepcopy(patch_neighb_list_dict[pu_id])
            if len(line_value_list) == 1:
                first_value = line_value_list.pop(0)
                line_string_list = [str(pu_id) + ':[' + str(first_value) + ']']
            elif len(line_value_list) > 1:
                line_string_list = [str(x) for x in line_value_list]
                first_value = line_string_list.pop(0)
                first_value_string = str(pu_id) + ':[' + first_value
                end_value = line_string_list.pop(len(line_string_list) - 1)
                end_value_string = end_value + ']'
                line_string_list.insert(0, first_value_string)
                line_string_list.append(end_value_string)
            else:
                line_string_list = [str(pu_id) + ':[]']
            output_writer.writerow(line_string_list)


def produce_mp_summed_dict(pu_dict):
    id_list = list(pu_dict.keys())
    count_list = [0] * len(id_list)
    init_tuple = zip(id_list, count_list)
    summed_dict = dict(init_tuple)

    return summed_dict


def produce_patch_results_dict(patch_results_dict, a_marxan_sol_file_path, before_patch_stats_dict, after_patch_stats_dict, cost_dict):
    bef_all_patch_count = before_patch_stats_dict['all_patch_count']
    bef_all_patch_area = before_patch_stats_dict['all_patch_area']
    bef_median_all_patch = before_patch_stats_dict['median_all_patch']
    bef_valid_patch_count = before_patch_stats_dict['valid_patch_count']
    bef_valid_patch_area = before_patch_stats_dict['valid_patch_area']
    bef_median_valid_patch = before_patch_stats_dict['median_valid_patch']

    aft_all_patch_count = after_patch_stats_dict['all_patch_count']
    aft_all_patch_area = after_patch_stats_dict['all_patch_area']
    aft_median_all_patch = after_patch_stats_dict['median_all_patch']
    aft_valid_patch_count = after_patch_stats_dict['valid_patch_count']
    aft_valid_patch_area = after_patch_stats_dict['valid_patch_area']
    aft_median_valid_patch = after_patch_stats_dict['median_valid_patch']

    portfolio_pu_cost = cost_dict['total_unit_cost']
    portfolio_bound_length = cost_dict['total_boundary_length']
    portfolio_bound_cost = cost_dict['total_boundary_cost']
    portfolio_total_cost = portfolio_pu_cost + portfolio_bound_cost

    name_string = path.basename(a_marxan_sol_file_path)
    result_list1 = [bef_all_patch_count, bef_all_patch_area, bef_median_all_patch, bef_valid_patch_count, bef_valid_patch_area, bef_median_valid_patch]
    result_list2 = [aft_all_patch_count, aft_all_patch_area, aft_median_all_patch, aft_valid_patch_count, aft_valid_patch_area, aft_median_valid_patch]
    result_list3 = [portfolio_pu_cost, portfolio_bound_length, portfolio_bound_cost, portfolio_total_cost]

    patch_results_dict[name_string] = result_list1 + result_list2 + result_list3

    return patch_results_dict


def make_run_zone_stats_dict(minpatch_data_dict, running_unit_dict):
    area_dictionary = minpatch_data_dict['area_dict']
    zone_dict = minpatch_data_dict['zone_dict']
    zone_type_dict = minpatch_data_dict['zone_dict']

    run_zone_stats_dict = dict()
    for aZone in zone_type_dict:
        run_zone_stats_dict[aZone] = [0, 0]

    for a_unit in running_unit_dict:
        pu_status = running_unit_dict[a_unit][1]
        if pu_status == 1 or pu_status == 2:
            pu_cost = running_unit_dict[a_unit][0]
            pu_area = area_dictionary[a_unit]
            pu_zone = zone_dict[a_unit][0]

            running_area_value = run_zone_stats_dict[pu_zone][0]
            running_cost_value = run_zone_stats_dict[pu_zone][1]
            running_area_value += pu_area
            running_cost_value += pu_cost

            run_zone_stats_dict[pu_zone] = [running_area_value, running_cost_value]

    return run_zone_stats_dict


def make_run_zone_feature_prop_stats_dict(minpatch_data_dict, running_unit_dict):
    target_dict = minpatch_data_dict['target_dict']
    abund_matrix_dict = minpatch_data_dict['abund_matrix_dict']
    zone_dict = minpatch_data_dict['zone_dict']
    zone_type_dict = minpatch_data_dict['zone_type_dict']

    run_zone_feature_stats_dict = make_run_zone_feature_stats_dict(target_dict, abund_matrix_dict, running_unit_dict, zone_dict, zone_type_dict)

    zone_prop_target_dict = dict()
    for zoneID in run_zone_feature_stats_dict:
        prop_target_dict = dict()
        feature_stats_dict = run_zone_feature_stats_dict[zoneID]
        for featID in feature_stats_dict:
            target_value = target_dict[featID][1]
            final_amount = feature_stats_dict[featID]
            try:
                prop_amount = final_amount / target_value
            except ArithmeticError:
                prop_amount = 0

            prop_target_dict[featID] = prop_amount

            zone_prop_target_dict[zoneID] = prop_target_dict

    return zone_prop_target_dict


def make_run_zone_feature_stats_dict(target_dict, abund_matrix_dict, unit_dict, zone_dict, zone_type_dict):
    feature_list = list(target_dict.keys())
    feature_list.sort()
    blank_list = [0] * len(feature_list)
    blank_feat_dict = dict(zip(feature_list, blank_list))

    run_zone_feature_stats_dict = dict()
    for zoneID in zone_type_dict:
        run_zone_feature_stats_dict[zoneID] = deepcopy(blank_feat_dict)

    for pu_id in unit_dict:
        pu_status = unit_dict[pu_id][1]
        if pu_status == 1 or pu_status == 2:
            pu_zone_id = zone_dict[pu_id][0]
            pu_abundance_dict = abund_matrix_dict[pu_id]

            for feat_id in pu_abundance_dict:
                feat_amount = pu_abundance_dict[feat_id]
                running_feat_amount = run_zone_feature_stats_dict[pu_zone_id][feat_id]
                running_feat_amount += feat_amount
                run_zone_feature_stats_dict[pu_zone_id][feat_id] = running_feat_amount

    return run_zone_feature_stats_dict


def update_mp_summed_dict(summed_dict, unit_dict):
    for pu_id in unit_dict:
        unit_list = unit_dict[pu_id]
        unit_status = unit_list[1]
        if unit_status == 1 or unit_status == 2:
            initial_count = summed_dict[pu_id]
            final_count = initial_count + 1
            summed_dict[pu_id] = final_count

    return summed_dict


def print_mp_summed_results(summed_dict, summed_file_path):
    with open(summed_file_path, 'w', newline='', encoding='utf-8') as summedFile:
        summed_results_writer = writer(summedFile)
        header_row = ['planning_unit', 'solution']
        summed_results_writer.writerow(header_row)

        pu_list = list(summed_dict.keys())
        pu_list.sort()
        for pu_id in pu_list:
            summed_results_writer.writerow([pu_id, summed_dict[pu_id]])


def print_mp_run_results(minpatch_data_dict, limbo_best_result_dict, file_path_string):
    with open(file_path_string, 'w', newline='', encoding='utf-8') as resultsFile:
        run_results_writer = writer(resultsFile)
        header_row = ['planning_unit', 'solution']
        run_results_writer.writerow(header_row)

        unit_dict = minpatch_data_dict['initial_unit_dict']
        for unitID in unit_dict:
            unit_status = unit_dict[unitID][1]
            unit_limbo_status = limbo_best_result_dict[unitID][1]
            if unit_status == 2 or unit_limbo_status == 1:
                final_unit_status = 1
            else:
                final_unit_status = 0

            run_results_writer.writerow([unitID, final_unit_status])


def print_mp_patch_stats(patch_results_dict, file_path_string):
    with open(file_path_string, 'w', newline='', encoding='utf-8') as patchStatsFile:
        patch_stats_writer = writer(patchStatsFile)
        header_row1 = ['File_name', 'Bef_AllPatchCount', 'Bef_AllPatchArea', 'Bef_medianAllPatch', 'Bef_ValidPatchCount', 'Bef_ValidPatchArea', 'Bef_medianValidPatch']
        header_row2 = ['Aft_AllPatchCount', 'Aft_AllPatchArea', 'Aft_medianAllPatch', 'Aft_ValidPatchCount', 'Aft_ValidPatchArea', 'Aft_medianValidPatch']
        header_row3 = ['PortfolioPUCost', 'PortfolioBoundLength', 'PortfolioBoundCost', 'PortfolioTotalCost']
        header_row = header_row1 + header_row2 + header_row3
        patch_stats_writer.writerow(header_row)
    
        for filenameString in patch_results_dict:
            patch_stats_writer.writerow([filenameString] + patch_results_dict[filenameString])


def print_mp_zone_stats(minpatch_data_dict, zone_stats_dict, zone_stats_base_file_name):
    zone_list = list(minpatch_data_dict['zone_type_dict'].keys())
    zone_list.sort()

    with open(zone_stats_base_file_name + '_zonestats.csv', 'w', newline='', encoding='utf-8') as zoneStatsFile:
        zone_stats_writer = writer(zoneStatsFile)
        zone_stats_header_list = make_mp_zone_stats_header_list(zone_list)
        zone_stats_writer.writerow(zone_stats_header_list)
    
        filename_list = zone_stats_dict.keys()
        filename_list.sort()
    
        for filenameString in filename_list:
            a_run_dict = zone_stats_dict[filenameString]
            results_list = [filenameString]
            for zoneID in a_run_dict:
                a_area = a_run_dict[zoneID][0]
                a_cost = a_run_dict[zoneID][1]
                results_list += [a_area, a_cost]
            zone_stats_writer.writerow(results_list)


def make_mp_zone_stats_header_list(zone_list):
    zone_stats_header_list = ['File_name']
    for zoneID in zone_list:
        zone_stats_header_list += ['Zone' + str(zoneID) + '_Area', 'Zone' + str(zoneID) + '_Cost']

    return zone_stats_header_list


def print_mp_zone_feature_prop_stats(minpatch_data_dict, zone_feature_prop_stats_dict, zone_stats_base_file_name):
    feat_list = minpatch_data_dict['target_dict'].keys()
    feat_list.sort()

    zone_feat_stats_header_list = ['File_name']
    for theFeat in feat_list:
        zone_feat_stats_header_list += ['Feat_' + str(theFeat)]

    zone_feature_output_dict = make_mp_zone_feature_output_dict(minpatch_data_dict, feat_list, zone_feature_prop_stats_dict)

    for zone_id in zone_feature_output_dict:
        with open(zone_stats_base_file_name + '_zonefeaturestat' + str(zone_id) + '.csv', 'w', newline='', encoding='utf-8') as zoneFeatStatsFile:
            zone_feat_stats_writer = writer(zoneFeatStatsFile)
            zone_details_dict = zone_feature_output_dict[zone_id]
            for runName in zone_details_dict:
                content_list = zone_details_dict[runName]
                zone_feat_stats_writer.writerow(content_list)


def make_mp_zone_feature_output_dict(minpatch_data_dict, feat_list, zone_feature_prop_stats_dict):
    run_list = zone_feature_prop_stats_dict.keys()
    run_list.sort()
    zone_list = list(minpatch_data_dict['zone_type_dict'].keys())
    zone_feature_output_dict = dict()

    for zone_id in zone_list:
        zone_details_dict = dict()
        for runName in run_list:
            content_list = [runName]
            a_feature_prop_result_dict = zone_feature_prop_stats_dict[runName][zone_id]
            for featID in feat_list:
                try:
                    prop_amount = a_feature_prop_result_dict[featID]
                except KeyError:
                    prop_amount = 0
                prop_string = str(round(prop_amount, 4))
                content_list += [prop_string]
            zone_details_dict[runName] = content_list
        zone_feature_output_dict[zone_id] = zone_details_dict

    return zone_feature_output_dict


def print_under_rep_features(setup_object, feat_amount_cons_dict, unmet_target_id_set, error_file_name):
    with open(error_file_name, 'w', newline='', encoding='utf-8') as errorFile:
        error_writer = writer(errorFile)
        error_writer.writerow(['Feat_ID', 'Total amount available in patches', 'Target'])
        for featID in unmet_target_id_set:
            row_list = list()
            row_list.append(featID)
            row_list.append(feat_amount_cons_dict[featID])
            row_list.append(setup_object.target_dict[featID][3])
            error_writer.writerow(row_list)
