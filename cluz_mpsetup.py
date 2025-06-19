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

from os import listdir, path, sep
from math import sqrt
from csv import reader

from .cluz_messages import run_yes_cancel_warning_dialog_box, clear_progress_bar, make_progress_bar, warning_message
from .cluz_messages import set_progress_bar_value
from .cluz_mpfunctions import pu_status_does_not_equal_excluded
from .cluz_mpoutputs import print_mp_patch_list_dict


def make_minpatch_data_dict(setup_object, minpatch_object):
    minpatch_data_dict = dict()
    input_path = setup_object.input_path
    setup_ok_bool = True

    pu_dict, xy_loc_dictionary = make_mp_pu_dicts(input_path + sep + 'pu.dat')
    minpatch_data_dict['initial_unit_dict'] = pu_dict
    minpatch_data_dict['xy_loc_dict'] = xy_loc_dictionary

    target_dict = make_mp_target_dict(input_path + sep + 'spec.dat')
    minpatch_data_dict['target_dict'] = make_mp_target_dict(input_path + sep + 'spec.dat')

    abund_matrix_dict = make_mp_abund_matrix_dict(input_path + sep + 'puvspr2.dat', target_dict, pu_dict)
    minpatch_data_dict['abund_matrix_dict'] = abund_matrix_dict

    bound_matrix_dict = make_bound_matrix_dict(input_path + sep + 'bound.dat', pu_dict)
    minpatch_data_dict['boundary_matrix_dict'] = bound_matrix_dict

    area_dict, zone_dict, zone_type_dict = make_mp_dicts(input_path + sep + 'minpatch.dat')
    minpatch_data_dict['area_dict'] = area_dict
    minpatch_data_dict['zone_dict'] = zone_dict
    minpatch_data_dict['zone_type_dict'] = zone_type_dict
    if len(zone_type_dict) > 1:
        minpatch_object.zonestats_bool = True
    else:
        minpatch_object.zonestats_bool = False

    setup_ok_bool = check_mp_pu_id_values_match(pu_dict, zone_dict, setup_ok_bool)
    files_to_be_created_list = make_mp_files_to_be_created_list(setup_object, minpatch_object, zone_type_dict)
    setup_ok_bool = check_mp_overwrite_existing_files(files_to_be_created_list, setup_ok_bool)
    setup_ok_bool = check_mp_files_can_be_saved(files_to_be_created_list, setup_ok_bool)
    setup_ok_bool = check_mp_patch_pu_id_file(setup_object, minpatch_data_dict, setup_ok_bool)
    if setup_ok_bool:
        add_patch_pu_id_dict = make_mp_patch_pu_id_dict(setup_object, minpatch_data_dict)
        minpatch_data_dict['add_patch_pu_id_dict'] = add_patch_pu_id_dict
        minpatch_data_dict = update_minpatch_data_dict_with_parameters(minpatch_object, minpatch_data_dict)

    return minpatch_data_dict, setup_ok_bool


def check_mp_pu_id_values_match(pu_dict, zone_dict, setup_ok_bool):
    if setup_ok_bool:
        if zone_dict.keys() != pu_dict.keys():
            warning_message('Input files error: ', 'the planning unit ID values in the unit.dat and MinPatch details file do not match, so MinPatch has been terminated.')
            setup_ok_bool = False
        
    return setup_ok_bool


def check_mp_overwrite_existing_files(files_to_be_created_list, setup_ok_bool):
    if setup_ok_bool:
        output_files_already_exist_bool = False
        for filePath in files_to_be_created_list:
            if path.isfile(filePath):
                output_files_already_exist_bool = True
        if output_files_already_exist_bool:
            warning_value = run_yes_cancel_warning_dialog_box('Overwrite files?', 'This will overwrite the existing files from a previous MinPatch analysis of the same Marxan files. Do you want to continue?')
            if warning_value is False:
                setup_ok_bool = False

    return setup_ok_bool


def check_mp_files_can_be_saved(files_to_be_created_list, setup_ok_bool):
    if setup_ok_bool:
        output_files_cannot_be_saved_bool = False
        try:
            for filePath in files_to_be_created_list:
                with open(filePath, 'wb') as f:
                    reader(f)
        except IOError:
            output_files_cannot_be_saved_bool = True

        if output_files_cannot_be_saved_bool:
            warning_message('Output files error: ', 'at least one of the required output files cannot be created. Please check that you have permission to write files in the specified output folder and that a file with the same name is not already open.')
            setup_ok_bool = False

    return setup_ok_bool


def check_mp_patch_pu_id_file(setup_object, minpatch_data_dict, setup_ok_bool):
    if setup_ok_bool:
        make_new_patch_pu_id_files_bool = check_patch_pu_id_file(setup_object, minpatch_data_dict)
        if make_new_patch_pu_id_files_bool:
            if radius_values_very_high(minpatch_data_dict):
                response_value = run_yes_cancel_warning_dialog_box('Radius values very high', 'At least one of the radius values specified in the MinPatch details file is more than 25% of the approximate height and/or width of the planning region. This could produce very large patches and make MinPatch run very slowly. Is that OK?')
                if response_value is False:
                    setup_ok_bool = False
            if setup_ok_bool:
                create_patch_pu_id_text_file(setup_object, minpatch_data_dict)

    return setup_ok_bool


def update_minpatch_data_dict_with_parameters(minpatch_object, minpatch_data_dict):
    minpatch_data_dict['bound_cost'] = minpatch_object.blm
    minpatch_data_dict['rem_small_patch'] = minpatch_object.removeBool
    minpatch_data_dict['add_patches'] = minpatch_object.addBool
    minpatch_data_dict['whittle_polish'] = minpatch_object.whittleBool
    minpatch_data_dict['patch_stats'] = True
    minpatch_data_dict['zone_stats'] = minpatch_object.zonestats_bool

    return minpatch_data_dict


def make_mp_files_to_be_created_list(setup_object, minpatch_object, zone_type_dict):
    patch_stats_file_path = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName + '_patchstats.csv'
    best_file_path = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName + '_best.txt'
    summed_file_path = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName + '_summed.txt'
    files_to_be_created_list = [patch_stats_file_path, best_file_path, summed_file_path]

    marxan_name_string = minpatch_object.marxanFileName + '_r'
    final_name_string = 'mp_' + marxan_name_string
    marxan_sol_file_list = make_mp_marxan_file_list(setup_object, marxan_name_string)

    for marxanSolFilePath in marxan_sol_file_list:
        run_output_file_path = marxanSolFilePath.replace(marxan_name_string, final_name_string)
        files_to_be_created_list.append(run_output_file_path)

    if minpatch_object.zonestats_bool:
        zone_stats_base_file_name = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName
        zone_stats_file_path = zone_stats_base_file_name + '_zonestats.csv'
        files_to_be_created_list.append(zone_stats_file_path)
        for zoneID in zone_type_dict.keys():
            zone_feat_stats_file_path = zone_stats_base_file_name + '_zonefeaturestat' + str(zoneID) + '.csv'
            files_to_be_created_list.append(zone_feat_stats_file_path)

    return files_to_be_created_list


def make_mp_marxan_file_list(setup_object, marxan_name_string):
    marxan_file_list = list()
    raw_list = listdir(setup_object.output_path)
    for a_string in raw_list:
        if a_string.startswith(marxan_name_string):
            b_string = setup_object.output_path + sep + a_string
            c_string = path.normpath(b_string)
            marxan_file_list.append(c_string)

    return marxan_file_list


def make_mp_pu_dicts(pu_loc_string):
    unit_dict = dict()
    xy_loc_dictionary = dict()

    with open(pu_loc_string, 'rt') as f:
        pu_reader = reader(f)
        next(pu_reader)
        for aRow in pu_reader:
            pu_id = int(aRow[0])
            pu_cost = float(aRow[1])
            pu_status = int(aRow[2])
            x_loc = float(aRow[3])
            y_loc = float(aRow[4])

            unit_dict[pu_id] = [pu_cost, pu_status]
            xy_loc_dictionary[pu_id] = [pu_status, x_loc, y_loc]

    return unit_dict, xy_loc_dictionary


def make_mp_target_dict(target_loc_string):
    target_dict = dict()

    with open(target_loc_string, 'rt') as f:
        target_reader = reader(f)
        next(target_reader)
        for aRow in target_reader:
            feat_id = int(aRow[0])
            feat_name = aRow[1]
            feat_target = float(aRow[2])
            feat_spf = float(aRow[3])
            feat_type = int(aRow[4])

            if feat_target > 0:
                target_dict[feat_id] = [feat_name, feat_target, feat_spf, feat_type]

    return target_dict


def make_mp_dicts(details_dat_path):
    area_dictionary = dict()
    zone_dict = dict()
    zone_type_dict = dict()

    with open(details_dat_path, 'rt') as f:
        zone_reader = reader(f)
        next(zone_reader)
        for aRow in zone_reader:
            pu_id = int(aRow[0])
            area_value = float(aRow[1])
            zone_id = int(aRow[2])
            zone_patch_area_value = float(aRow[3])
            zone_radius_value = float(aRow[4])

            area_dictionary[pu_id] = area_value
            zone_dict[pu_id] = [zone_id, zone_patch_area_value, zone_radius_value]
            zone_type_dict[zone_id] = [zone_patch_area_value, zone_radius_value]

    return area_dictionary, zone_dict, zone_type_dict


def check_patch_pu_id_file(setup_object, minpatch_data_dict):
    zone_type_dict = minpatch_data_dict['zone_type_dict']
    zone_type_radius_dict = dict()
    for zoneID in zone_type_dict:
        zone_radius = zone_type_dict[zoneID][1]
        zone_type_radius_dict[zoneID] = zone_radius

    make_new_patch_pu_id_files_bool = False
    patch_pu_id_file_path = setup_object.input_path + sep + 'patchPUID.dat'
    try:
        patch_pu_id_zone_radius_dict = make_patch_pu_id_zone_radius_dict(patch_pu_id_file_path)
    except IOError:
        make_new_patch_pu_id_files_bool = True
        patch_pu_id_zone_radius_dict = dict()

    if zone_type_radius_dict != patch_pu_id_zone_radius_dict:
        make_new_patch_pu_id_files_bool = True

    return make_new_patch_pu_id_files_bool


def make_patch_pu_id_zone_radius_dict(patch_pu_id_file_path):
    patch_pu_id_zone_radius_dict = dict()
    with open(patch_pu_id_file_path, 'rt') as f:
        patch_reader = reader(f)
        for aRow in patch_reader:
            first_text_block = aRow[0]
            if '***' in first_text_block:
                if '*** Patch search distance for Zone' in first_text_block:
                    raw_zone_text_list = first_text_block.split('=')
                    zone_id = int(raw_zone_text_list[0].replace('*** Patch search distance for Zone ', ''))
                    zone_radius_value = float(raw_zone_text_list[1])
                    patch_pu_id_zone_radius_dict[zone_id] = zone_radius_value
            else:
                break

    return patch_pu_id_zone_radius_dict


def make_mp_patch_pu_id_dict(setup_object, minpatch_data_dict):
    patch_pu_id_path_name = setup_object.input_path + sep + 'patchPUID.dat'
    area_dict = minpatch_data_dict['area_dict']
    zone_dict = minpatch_data_dict['zone_dict']

    patch_pu_id_dict = dict()

    with open(patch_pu_id_path_name, 'rt') as f:
        patch_reader = reader(f)
        for aRow in patch_reader:
            if '***' not in aRow[0]:
                pu_id, patch_id_list = make_patch_id_details_from_file_row(aRow)
                if is_patch_bigger_than_minimum_size(pu_id, patch_id_list, area_dict, zone_dict):
                    patch_pu_id_dict[pu_id] = patch_id_list
                else:
                    patch_pu_id_dict[pu_id] = []

    return patch_pu_id_dict


def make_patch_id_details_from_file_row(a_row):
    raw_first_two_values_list = a_row.pop(0).split(':[')
    pu_id = int(raw_first_two_values_list[0])
    first_patch_value = raw_first_two_values_list[1]
    if is_patch_pu_id_list_empty(first_patch_value):
        patch_id_list = list()
    else:
        a_row.insert(0, first_patch_value)
        end_raw_string = a_row.pop(len(a_row) - 1)
        end_string = end_raw_string.replace(']', '')
        a_row.append(end_string)
        patch_id_list = [int(aString) for aString in a_row]

    return pu_id, patch_id_list


def is_patch_pu_id_list_empty(first_patch_value):
    patch_pu_id_list_is_empty = False
    if first_patch_value == ']':
        patch_pu_id_list_is_empty = True

    return patch_pu_id_list_is_empty


def is_patch_bigger_than_minimum_size(pu_id, patch_id_list, area_dict, zone_dict):
    patch_is_bigger_than_minimum = False
    min_patch_size = zone_dict[pu_id][1]
    running_patch_size = area_dict[pu_id]
    for patch_pu_id in patch_id_list:
        patch_pu_size = area_dict[patch_pu_id]
        running_patch_size += patch_pu_size

    if running_patch_size >= min_patch_size:
        patch_is_bigger_than_minimum = True

    return patch_is_bigger_than_minimum


def make_bound_matrix_dict(boundary_location_string, pu_dict):
    bound_matrix_dict = dict()
    pu_list = list(pu_dict.keys())
    for aNum in pu_list:
        bound_matrix_dict[aNum] = dict()

    with open(boundary_location_string, 'rt') as f:
        bound_reader = reader(f)
        next(bound_reader)
        for aRow in bound_reader:
            id1_value = int(aRow[0])
            id2_value = int(aRow[1])
            bound_value = float(aRow[2])

            bound_dict1 = bound_matrix_dict[id1_value]
            bound_dict1[id2_value] = bound_value
            bound_dict2 = bound_matrix_dict[id2_value]
            bound_dict2[id1_value] = bound_value

    return bound_matrix_dict


def make_mp_abund_matrix_dict(abundance_location_string, target_dict, pu_dict):
    abund_matrix_dict = dict()
    pu_list = pu_dict.keys()
    feat_set = set(target_dict.keys())
    for aNum in pu_list:
        abund_matrix_dict[aNum] = dict()

    with open(abundance_location_string, 'rt') as f:
        abund_reader = reader(f)
        next(abund_reader)
        for aRow in abund_reader:
            feat_id = int(aRow[0])
            if feat_id in feat_set:
                pu_id = int(aRow[1])
                feat_amount = float(aRow[2])

                pu_abund_dict = abund_matrix_dict[pu_id]
                pu_abund_dict[feat_id] = feat_amount

    return abund_matrix_dict


def create_patch_pu_id_text_file(setup_object, minpatch_data_dict):
    xy_loc_dict = minpatch_data_dict['xy_loc_dict']
    zone_dict = minpatch_data_dict['zone_dict']
    zone_type_dict = minpatch_data_dict['zone_type_dict']
    unit_dict = minpatch_data_dict['initial_unit_dict']
    bound_matrix_dict = minpatch_data_dict['boundary_matrix_dict']
    patch_list_dict = dict()

    progress_bar = make_progress_bar('Making patchPUID.dat file in input folder')
    row_total_count = len(unit_dict)
    row_count = 1

    for pu_id in unit_dict:
        set_progress_bar_value(progress_bar, row_count, row_total_count)
        row_count += 1

        pu_status, pu_x_value, pu_y_value = xy_loc_dict[pu_id]
        zone_id, zone_patch_area_value, zone_patch_radius_value = zone_dict[pu_id]
        pu_id_patch_set = set()
        if pu_status_does_not_equal_excluded(unit_dict, pu_id):
            candidate_pu_id_patch_set = set(bound_matrix_dict[pu_id].keys())
            already_tested_pu_id_patch_set = set()
            already_tested_pu_id_patch_set.add(pu_id)
            while len(candidate_pu_id_patch_set) > 0:
                test_candidate_pu_id = candidate_pu_id_patch_set.pop()
                test_candidate_pu_status, test_candidate_pu_x_value, test_candidate_pu_y_value = xy_loc_dict[test_candidate_pu_id]
                if pu_status_does_not_equal_excluded(unit_dict, test_candidate_pu_id):
                    if is_pu_centroid_within_patch_radius(pu_x_value, pu_y_value, test_candidate_pu_x_value, test_candidate_pu_y_value, zone_patch_radius_value):
                        pu_id_patch_set.add(test_candidate_pu_id)
                        test_candidate_pu_id_patch_set = set(bound_matrix_dict[test_candidate_pu_id].keys())
                        new_candidate_pu_id_patch_set = test_candidate_pu_id_patch_set.difference(already_tested_pu_id_patch_set)
                        candidate_pu_id_patch_set = candidate_pu_id_patch_set.union(new_candidate_pu_id_patch_set)
                    already_tested_pu_id_patch_set.add(test_candidate_pu_id)

            pu_id_patch_list = list(pu_id_patch_set)
            pu_id_patch_list.sort()
            patch_list_dict[pu_id] = pu_id_patch_list

    clear_progress_bar()
    print_mp_patch_list_dict(patch_list_dict, zone_type_dict, setup_object.input_path + sep + 'patchPUID.dat')


def is_pu_centroid_within_patch_radius(a_x_value, a_y_value, b_x_value, b_y_value, patch_radius_value):
    centroid_within_patch_radius = False
    x_diff = a_x_value - b_x_value
    y_diff = a_y_value - b_y_value
    x_diff_square = pow(x_diff, 2)
    y_diff_square = pow(y_diff, 2)
    centr_dist = sqrt(x_diff_square + y_diff_square)
    if centr_dist <= patch_radius_value:
        centroid_within_patch_radius = True

    return centroid_within_patch_radius


def radius_values_very_high(minpatch_data_dict):
    check_bool = False
    min_x, max_x, min_y, max_y = return_min_max_xy_list(minpatch_data_dict)
    x_extent = max_x - min_x
    y_extent = max_y - min_y
    highest_radius_value = return_highest_radius_value(minpatch_data_dict)
    if highest_radius_value / x_extent > 0.25 or highest_radius_value / y_extent > 0.25:
        check_bool = True

    return check_bool


def return_min_max_xy_list(minpatch_data_dict):
    xy_loc_dictionary = minpatch_data_dict['xy_loc_dict']
    min_x, max_x, min_y, max_y = ['blank', 'blank', 'blank', 'blank']
    for pu_id in xy_loc_dictionary:
        pu_status, x_loc_string, y_loc_string = xy_loc_dictionary[pu_id]
        x_loc = float(x_loc_string)
        y_loc = float(y_loc_string)
        if min_x == 'blank' or x_loc < min_x:
            min_x = x_loc
        if max_x == 'blank' or x_loc > max_x:
            max_x = x_loc
        if min_y == 'blank' or y_loc < min_y:
            min_y = y_loc
        if max_y == 'blank' or y_loc > max_y:
            max_y = y_loc

    min_max_xy_list = [min_x, max_x, min_y, max_y]

    return min_max_xy_list


def return_highest_radius_value(minpatch_data_dict):
    zone_type_dict = minpatch_data_dict['zone_type_dict']
    highest_radius_value = -1
    for zoneID in zone_type_dict:
        zone_radius_value = zone_type_dict[zoneID][1]
        if zone_radius_value > highest_radius_value:
            highest_radius_value = zone_radius_value

    return highest_radius_value
