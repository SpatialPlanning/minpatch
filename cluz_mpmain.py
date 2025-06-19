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
from os import path, sep

from .cluz_mpoutputs import make_mp_patch_stats_dict, make_run_zone_feature_prop_stats_dict, print_mp_summed_results, produce_mp_summed_dict, print_mp_patch_stats, update_mp_summed_dict, print_mp_zone_stats, produce_patch_results_dict, print_mp_run_results, print_mp_zone_feature_prop_stats, make_run_zone_stats_dict
from .cluz_mpoutputs import print_under_rep_features

from .cluz_mpfunctions import make_mp_patch_dict, make_mp_cost_dict, rem_small_patches_from_unit_dict, create_mp_running_unit_dictionary, add_conserved_pus, run_sim_whittle, add_mp_patches
from .cluz_messages import success_message, critical_message
from .cluz_display import remove_previous_min_patch_layers, display_graduated_layer, reload_pu_layer, display_best_output
from .cluz_mpsetup import make_mp_marxan_file_list
from .cluz_functions5 import add_best_marxan_output_to_pu_shapefile, add_summed_marxan_output_to_pu_shapefile


def run_min_patch(setup_object, minpatch_object, minpatch_data_dict):
    marxan_name_string = minpatch_object.marxanFileName + '_r'
    final_name_string = 'mp_' + marxan_name_string
    marxan_sol_file_list = make_mp_marxan_file_list(setup_object, marxan_name_string)

    pre_marxan_unit_dict = minpatch_data_dict['initial_unit_dict']
    summed_sol_dict = produce_mp_summed_dict(pre_marxan_unit_dict)
    patch_results_dict = dict()
    zone_stats_dict = dict()
    zone_feature_prop_stats_dict = dict()

    best_portfolio_cost = -1
    continue_bool = True

    for marxanSolFilePath in marxan_sol_file_list:
        running_unit_dict = create_mp_running_unit_dictionary(minpatch_data_dict, marxanSolFilePath)
        patch_dict = make_mp_patch_dict(running_unit_dict, minpatch_data_dict)

        if minpatch_data_dict['patch_stats'] and continue_bool:
            before_patch_stats_dict = make_mp_patch_stats_dict(patch_dict, minpatch_data_dict)

        if minpatch_data_dict['rem_small_patch'] and continue_bool:
            running_unit_dict = rem_small_patches_from_unit_dict(minpatch_data_dict, running_unit_dict, patch_dict, marxanSolFilePath)

        if minpatch_data_dict['add_patches'] and continue_bool:
            running_unit_dict, feat_amount_cons_dict, unmet_target_id_set, continue_bool = add_mp_patches(setup_object, minpatch_data_dict, running_unit_dict, marxanSolFilePath)
            if len(unmet_target_id_set) > 0:
                error_file_name = marxanSolFilePath.replace(marxan_name_string, final_name_string).replace('.txt', '_errror.csv')
                critical_message('Target error: ', 'targets for ' + str(len(unmet_target_id_set)) + ' features cannot be met. This occurs when there is not enough of the relevant features found in patches with the specified minimum area. Details have been saved in the file ' + error_file_name + '. MinPatch has been terminated.')
                print_under_rep_features(setup_object, feat_amount_cons_dict, unmet_target_id_set, error_file_name)
                continue_bool = False

        if minpatch_data_dict['whittle_polish'] and continue_bool:
            running_unit_dict = run_sim_whittle(setup_object, running_unit_dict, minpatch_data_dict, marxanSolFilePath)

        running_unit_dict = add_conserved_pus(running_unit_dict, minpatch_data_dict)

        if minpatch_data_dict['patch_stats'] and continue_bool:
            patch_dict = make_mp_patch_dict(running_unit_dict, minpatch_data_dict)
            after_patch_stats_dict = make_mp_patch_stats_dict(patch_dict, minpatch_data_dict)

        if continue_bool:
            output_file_path = marxanSolFilePath.replace(marxan_name_string, final_name_string)
            print_mp_run_results(minpatch_data_dict, running_unit_dict, output_file_path)

            cost_dict = make_mp_cost_dict(minpatch_data_dict, running_unit_dict)
            total_cost = cost_dict['total_boundary_cost'] + cost_dict['total_unit_cost']

            if minpatch_data_dict['patch_stats']:
                patch_results_dict = produce_patch_results_dict(patch_results_dict, marxanSolFilePath, before_patch_stats_dict, after_patch_stats_dict, cost_dict)

            if minpatch_data_dict['zone_stats']:
                zone_name_string = path.basename(marxanSolFilePath)
                zone_stats_dict[zone_name_string] = make_run_zone_stats_dict(minpatch_data_dict, running_unit_dict)
                zone_feature_prop_stats_dict[zone_name_string] = make_run_zone_feature_prop_stats_dict(minpatch_data_dict, running_unit_dict)

            if best_portfolio_cost == -1:
                best_portfolio_cost = total_cost
                best_portfolio = deepcopy(running_unit_dict)

            if best_portfolio_cost != -1 and total_cost < best_portfolio_cost:
                best_portfolio_cost = total_cost
                best_portfolio = deepcopy(running_unit_dict)

            summed_dict = update_mp_summed_dict(summed_sol_dict, running_unit_dict)

    if continue_bool:
        best_file_name = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName + '_best.txt'
        print_mp_run_results(minpatch_data_dict, best_portfolio, best_file_name)

        summed_file_name = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName + '_summed.txt'
        print_mp_summed_results(summed_dict, summed_file_name)

        if minpatch_data_dict['patch_stats']:
            patchstats_file_name = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName + '_patchstats.csv'
            print_mp_patch_stats(patch_results_dict, patchstats_file_name)

        if minpatch_data_dict['zone_stats']:
            zone_stats_base_file_name = setup_object.output_path + sep + 'mp_' + minpatch_object.marxanFileName
            print_mp_zone_stats(minpatch_data_dict, zone_stats_dict, zone_stats_base_file_name)
            print_mp_zone_feature_prop_stats(minpatch_data_dict, zone_feature_prop_stats_dict, zone_stats_base_file_name)

        add_best_marxan_output_to_pu_shapefile(setup_object, best_file_name, 'MP_Best')
        add_summed_marxan_output_to_pu_shapefile(setup_object, summed_file_name, 'MP_SF_Scr')

        reload_pu_layer(setup_object)
        remove_previous_min_patch_layers()
        best_layer_name = 'MP Best (' + minpatch_object.marxanFileName + ')'
        summed_layer_name = 'MP SF_Score (' + minpatch_object.marxanFileName + ')'
        display_best_output(setup_object, 'MP_Best', best_layer_name)
        display_graduated_layer(setup_object, 'MP_SF_Scr', summed_layer_name, 1)  # 1 is SF legend code

        success_message('MinPatch results', 'MinPatch has completed the analysis and the results files are in the specified output folder.')
