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

from qgis.core import QgsVectorLayer, QgsSpatialIndex, QgsField
from qgis.PyQt.QtCore import QVariant

from copy import deepcopy
from csv import reader, writer
from math import exp, log, sqrt
from os import path, sep
from statistics import median
from time import sleep

from .cluz_messages import clear_progress_bar, empty_polygon_pu_id_set_error_message, make_progress_bar
from .cluz_messages import set_progress_bar_value, warning_message, critical_message,success_message
from .cluz_make_file_dicts import write_bound_dat_file

# Produce Marxan input files ##########################################################################################


def create_spec_dat_file(setup_object):
    spec_dat_path_name = setup_object.input_path + sep + 'spec.dat'
    with open(spec_dat_path_name, 'w', newline='', encoding='utf-8') as out_file:
        spec_dat_writer = writer(out_file)
        spec_dat_writer.writerow(['id', 'name', 'target', 'spf', 'type'])
    
        target_dict = setup_object.target_dict
        feat_list = list(target_dict.keys())
        feat_list.sort()

        progress_bar = make_progress_bar('Making a new spec.dat file')
        row_total_count = len(feat_list)
        row_count = 1

        for aFeat in feat_list:
            set_progress_bar_value(progress_bar, row_count, row_total_count)
            row_count += 1

            feat_list = target_dict[aFeat]
            raw_feat_name = feat_list[0]
            change_bool, feat_name = convert_feat_name_by_changing_incompatible_text_characters(raw_feat_name)
            feat_target = feat_list[3]
            feat_spf = feat_list[2]
            feat_type = feat_list[1]
            spec_dat_writer.writerow([aFeat, feat_name, feat_target, feat_spf, feat_type])
    clear_progress_bar()


def convert_feat_name_by_changing_incompatible_text_characters(raw_feat_name):
    change_bool = False
    feat_name = raw_feat_name.replace(' ', '_')
    feat_name = feat_name.replace('.', '')

    if raw_feat_name != feat_name:
        change_bool = True

    return change_bool, feat_name


def create_pu_dat_file(setup_object):
    decimal_places = setup_object.decimal_places
    pu_dat_path_name = setup_object.input_path + sep + 'pu.dat'

    pu_layer = QgsVectorLayer(setup_object.pu_path, 'Planning units', 'ogr')
    pu_features = pu_layer.getFeatures()
    pu_id_field = pu_layer.fields().indexFromName('Unit_ID')
    pu_cost_field = pu_layer.fields().indexFromName('Cost')
    pu_status_field = pu_layer.fields().indexFromName('Status')

    progress_bar = make_progress_bar('Making a new pu.dat file')
    poly_count = 1
    poly_total_count = pu_layer.featureCount()

    with open(pu_dat_path_name, 'w', newline='', encoding='utf-8') as out_file:
        pu_dat_writer = writer(out_file)
        pu_dat_writer.writerow(['id', 'cost', 'status', 'xloc', 'yloc'])
        pu_status_dict = {'Available': 0, 'Earmarked': 2, 'Conserved': 2, 'Excluded': 3}

        for puFeature in pu_features:
            set_progress_bar_value(progress_bar, poly_count, poly_total_count)
            poly_count += 1
            pu_dat_row_list = make_pu_dat_row_list(puFeature, pu_status_dict, pu_id_field, pu_cost_field, pu_status_field, decimal_places)
            pu_dat_writer.writerow(pu_dat_row_list)
    clear_progress_bar()


def make_pu_dat_row_list(pu_feature, pu_status_dict, pu_id_field, pu_cost_field, pu_status_field, decimal_places):
    pu_attributes = pu_feature.attributes()
    pu_id = pu_attributes[pu_id_field]
    pu_cost = pu_attributes[pu_cost_field]
    pu_status = pu_attributes[pu_status_field]
    pu_status_code = pu_status_dict[pu_status]

    pu_centroid = pu_feature.geometry().centroid()
    raw_x_coord = pu_centroid.asPoint().x()
    x_coord = round(float(raw_x_coord), decimal_places)
    x_coord = format(x_coord, "." + str(decimal_places) + "f")

    raw_y_coord = pu_centroid.asPoint().y()
    y_coord = round(float(raw_y_coord), decimal_places)
    y_coord = format(y_coord, "." + str(decimal_places) + "f")

    pu_dat_row_list = [pu_id, pu_cost, pu_status_code, x_coord, y_coord]

    return pu_dat_row_list


def create_bound_dat_file(setup_object, ext_edge_bool):
    vertex_precision = determine_vertex_precision(setup_object)

    pu_layer = QgsVectorLayer(setup_object.pu_path, 'Planning units', 'ogr')
    pu_id_field_index = pu_layer.fields().indexFromName('Unit_ID')
    pu_id_geom_dict = make_pu_id_geom_dict(pu_layer, pu_id_field_index)
    vertex_list, empty_polygon_pu_id_set = make_vertex_list(pu_id_geom_dict, vertex_precision)
    vertex_list.sort()
    bound_results_dict = make_bound_result_dict(vertex_list)
    if len(empty_polygon_pu_id_set) > 0:
        empty_polygon_pu_id_set_error_message(empty_polygon_pu_id_set)

    write_bound_dat_file(setup_object, bound_results_dict, ext_edge_bool)


def determine_vertex_precision(setup_object):
    pu_layer = QgsVectorLayer(setup_object.pu_path, 'Planning units', 'ogr')
    pu_layer_extent = pu_layer.extent()
    x_range = pu_layer_extent.xMaximum() - pu_layer_extent.xMinimum()
    y_range = pu_layer_extent.yMaximum() - pu_layer_extent.yMinimum()
    min_range_value = min([x_range, y_range])
    if min_range_value > 10:
        vertex_precision = 3
    elif min_range_value < 0:
        vertex_precision = 8
    else:
        vertex_precision = 5

    return vertex_precision


def make_pu_id_geom_dict(pu_layer, pu_id_field_index):
    pu_id_geom_dict = dict()

    progress_bar = make_progress_bar('Processing planning unit shapefile spatial data')
    poly_count = 1
    poly_total_count = pu_layer.featureCount()

    for aPolygon in pu_layer.getFeatures():
        set_progress_bar_value(progress_bar, poly_count, poly_total_count)
        poly_count += 1
        pu_id_geom_dict[aPolygon.attributes()[pu_id_field_index]] = aPolygon.geometry()
    clear_progress_bar()

    return pu_id_geom_dict


def make_vertex_list(pu_id_geom_dict, vertex_precision):
    vertex_list = list()

    progress_bar = make_progress_bar('Extracting the vertex data from the planning unit shapefile')
    pu_running_count = 1
    num_pus = len(pu_id_geom_dict)

    empty_polygon_pu_id_set = set()
    for pu_id in pu_id_geom_dict:
        set_progress_bar_value(progress_bar, pu_running_count, num_pus)

        pu_geom = pu_id_geom_dict[pu_id]
        pu_vertex_set = make_new_pu_vertex_set(pu_geom, vertex_precision)
        if len(pu_vertex_set) == 0:
            empty_polygon_pu_id_set.add(pu_id)
        else:
            for aVertex in pu_vertex_set:
                vertex_list.append((aVertex, pu_id))

        pu_running_count += 1

    clear_progress_bar()

    return vertex_list, empty_polygon_pu_id_set


def make_bound_result_dict(vertex_list):
    bound_results_dict = dict()
    row_num = 0
    list_length = len(vertex_list) - 1

    progress_bar = make_progress_bar('Extracting the vertex data from the planning unit shapefile')
    tot_row_num = len(vertex_list)

    while row_num < list_length:
        set_progress_bar_value(progress_bar, row_num, tot_row_num)

        (vertex_a, pu_id_a) = vertex_list[row_num]
        (vertex_b, pu_id_b) = vertex_list[row_num + 1]

        if vertex_a == vertex_b:
            pu_dict_key = (pu_id_a, pu_id_b)
            bound_results_dict[pu_dict_key] = return_running_length_value(bound_results_dict, vertex_a, pu_dict_key)
            row_num += 2
        else:
            pu_dict_key = (pu_id_a, pu_id_a)
            bound_results_dict[pu_dict_key] = return_running_length_value(bound_results_dict, vertex_a, pu_dict_key)
            row_num += 1
    clear_progress_bar()

    return bound_results_dict


def return_running_length_value(bound_results_dict, a_vertex, pu_dict_key):
    vertex_length = calc_vertex_length(a_vertex)
    try:
        running_length_value = bound_results_dict[pu_dict_key]
        running_length_value += vertex_length
    except KeyError:
        running_length_value = vertex_length

    return running_length_value


def make_new_pu_vertex_set(pu_geom, vertex_precision):
    a_poly_point_list = list()

    if pu_geom.isMultipart():
        polygon_list = pu_geom.asMultiPolygon()
    else:
        polygon_list = [pu_geom.asPolygon()]

    for a_polygon in polygon_list:
        for a_ring in a_polygon:
            poly_xy_list = list()
            for aQgsPointXY in a_ring:
                x_value = round(aQgsPointXY.x(), vertex_precision)
                y_value = round(aQgsPointXY.y(), vertex_precision)
                poly_xy_list.append((x_value, y_value))

            a_poly_point_list.append(poly_xy_list)

    pu_vertex_set = convert_polygon_point_list2_vertex_set(a_poly_point_list)

    return pu_vertex_set


def calc_vertex_length(a_vertex):
    (x1, y1, x2, y2) = a_vertex
    x_length = x2 - x1
    y_length = y2 - y1
    vertex_length = sqrt(x_length**2 + y_length**2)

    return vertex_length


def convert_polygon_point_list2_vertex_set(poly_point_list):  # This deals with multi polygon planning units
    vertex_set = set()
    for aPolygonPointList in poly_point_list:
        list_length = len(aPolygonPointList)
        for aNumber in range(0, list_length - 1):
            x1 = aPolygonPointList[aNumber][0]
            y1 = aPolygonPointList[aNumber][1]
            x2 = aPolygonPointList[aNumber + 1][0]
            y2 = aPolygonPointList[aNumber + 1][1]
            if x1 > x2:
                final_x1 = x2
                final_x2 = x1
            else:
                final_x1 = x1
                final_x2 = x2
            if y1 > y2:
                final_y1 = y2
                final_y2 = y1
            else:
                final_y1 = y1
                final_y2 = y2
            vec_tuple = (final_x1, final_y1, final_x2, final_y2)
            vertex_set.add(vec_tuple)

    return vertex_set


def report_output_success_message(message_string_list):
    if len(message_string_list) > 0:
        message_string = ''
        for aString in message_string_list:
            message_string += aString + ', '
        final_message_string = message_string[:-2]
        success_message('Marxan files:', 'the following files have been produced: ' + final_message_string)


# Marxan dialog  #####################################################

def return_output_name(setup_object):
    old_output_name = setup_object.output_name
    output_path = setup_object.output_path
    old_output_best_name = output_path + sep + old_output_name + '_best.txt'

    old_output_name_stem = ''
    num_value_bool = True
    for aNum in range(len(old_output_name), 0, -1):
        a_char = old_output_name[aNum - 1]
        try:
            int(a_char)
        except ValueError:
            num_value_bool = False
        if num_value_bool is False:
            old_output_name_stem = a_char + old_output_name_stem

    if path.isfile(old_output_best_name):
        name_suffix = 1
        new_name = output_path + sep + old_output_name_stem + str(name_suffix) + '_best.txt'
        while path.isfile(new_name):
            name_suffix += 1
            new_name = output_path + sep + old_output_name_stem + str(name_suffix) + '_best.txt'

        output_name = old_output_name_stem + str(name_suffix)
    else:
        output_name = old_output_name

    return output_name


def check_num_iter_para_dict(num_iter):
    check_bool = True
    try:
        int(num_iter)
        if int(num_iter) < 10000:
            warning_message('Input error', 'The number of iterations must be higher than 10000 because it must be be higher than the NUMTEMP value used in Marxan (see the Marxan manual for more details).')
            check_bool = False
    except ValueError:
        warning_message('Input error', 'The number of iterations must be an integer')
        check_bool = False

    return check_bool


def check_num_runs_para_dict(num_run, check_bool):
    try:
        int(num_run)
        if int(num_run) < 1:
            warning_message('Input error', 'The number of runs must be 1 or a larger whole number')
            check_bool = False
    except ValueError:
        warning_message('Input error', 'The number of runs must be an integer.')
        check_bool = False

    return check_bool


def check_blm_value_para_dict(blm_value, check_bool):
    try:
        float(blm_value)
        if float(blm_value) < 0:
            warning_message('Input error', 'The boundary length modifier must be a non-negative number.')
            check_bool = False
    except ValueError:
        warning_message('Input error', 'The boundary length modifier must be a non-negative number.')
        check_bool = False

    return check_bool


def check_missing_prop_value_para_dict(missing_prop, check_bool):
    try:
        float(missing_prop)
        if float(missing_prop) < 0 or float(missing_prop) > 1:
            check_bool = False
            warning_message('Input error', 'The species proportion value must be a number between 0 and 1.')
    except ValueError:
        check_bool = False
        warning_message('Input error', 'The species proportion value must be a number between 0 and 1.')

    return check_bool


def check_initial_prop_value_para_dict(initial_prop, check_bool):
    try:
        float(initial_prop)
        if float(initial_prop) < 0 or float(initial_prop) > 1:
            check_bool = False
            warning_message('Input error', 'The proportion of planning units randomly included at the beginning of each run must be a number between 0 and 1.')
    except ValueError:
        check_bool = False
        warning_message('Input error', 'The proportion of planning units randomly included at the beginning of each run must be a number between 0 and 1.')

    return check_bool


def check_permission_to_use_marxan_folder_para_dict(marxan_parameter_dict, marxan_input_values_bool):
    marxan_path_text = marxan_parameter_dict['marxan_path']
    marxan_folder = path.dirname(marxan_path_text)
    marxan_input_path = marxan_folder + sep + 'input.dat'
    try:
        with open(marxan_input_path, 'w', newline='', encoding='utf-8') as marxanFile:
            marxan_writer = writer(marxanFile)
    except PermissionError:
        critical_message('Permission problem', 'You do not have permission to save files in the specified Marxan folder. CLUZ needs this to create input.dat and .bat files in the Marxan folder. Please move Marxan to a folder where you do have permission to save files.')
        marxan_input_values_bool = False

    return marxan_input_values_bool


def make_marxan_input_file(setup_object, marxan_parameter_dict):
    if marxan_parameter_dict['extra_outputs_bool']:
        extra_output_value = '2'
    else:
        extra_output_value = '0'
    if path.isfile(marxan_parameter_dict['marxan_path']):
        write_marxan_input_file(setup_object, marxan_parameter_dict, extra_output_value)


def write_marxan_input_file(setup_object, marxan_parameter_dict, extra_output_value):
    with open(marxan_parameter_dict['marxan_setup_path'], 'w', newline='', encoding='utf-8') as marxanFile:
        marxan_writer = writer(marxanFile)

        header1 = 'Input file for Marxan program, written by Ian Ball, Hugh Possingham and Matt Watts.'
        header2 = 'This file was generated using CLUZ, written by Bob Smith'
        marxan_writer.writerow([header1])
        marxan_writer.writerow([header2])
        marxan_writer.writerow([])

        marxan_writer.writerow(['General Parameters'])
        marxan_writer.writerow(['VERSION 0.1'])
        marxan_writer.writerow(['BLM ' + str(marxan_parameter_dict['blm_value'])])
        marxan_writer.writerow(['PROP  ' + str(marxan_parameter_dict['initial_prop'])])
        marxan_writer.writerow(['RANDSEED -1'])
        marxan_writer.writerow(['BESTSCORE  10'])
        marxan_writer.writerow(['NUMREPS ' + str(marxan_parameter_dict['num_run'])])
        marxan_writer.writerow([])

        marxan_writer.writerow(['Annealing Parameters'])
        marxan_writer.writerow(['NUMITNS ' + str(marxan_parameter_dict['num_iter'])])
        marxan_writer.writerow(['STARTTEMP -1.00000000000000E+0000'])
        marxan_writer.writerow(['COOLFAC  6.00000000000000E+0000'])
        marxan_writer.writerow(['NUMTEMP 10000'])
        marxan_writer.writerow([])

        marxan_writer.writerow(['Cost Threshold'])
        marxan_writer.writerow(['COSTTHRESH  0.00000000000000E+0000'])
        marxan_writer.writerow(['THRESHPEN1  1.40000000000000E+0001'])
        marxan_writer.writerow(['THRESHPEN2  1.00000000000000E+0000'])
        marxan_writer.writerow([])

        marxan_writer.writerow(['Input Files'])
        marxan_writer.writerow(['INPUTDIR ' + setup_object.input_path])
        marxan_writer.writerow(['SPECNAME ' + marxan_parameter_dict['spec_name']])  # Normally 'spec.dat' apart from in Calibrate function
        marxan_writer.writerow(['PUNAME pu.dat'])
        marxan_writer.writerow(['PUVSPRNAME puvspr2.dat'])
        marxan_writer.writerow(['BOUNDNAME bound.dat'])
        marxan_writer.writerow([])

        marxan_writer.writerow(['Save Files'])
        marxan_writer.writerow(['SCENNAME ' + marxan_parameter_dict['output_name']])
        marxan_writer.writerow(['SAVERUN ' + extra_output_value])
        marxan_writer.writerow(['SAVEBEST 2'])
        marxan_writer.writerow(['SAVESUMMARY 2'])
        marxan_writer.writerow(['SAVESCEN ' + extra_output_value])
        marxan_writer.writerow(['SAVETARGMET 2'])
        marxan_writer.writerow(['SAVESUMSOLN 2'])
        marxan_writer.writerow(['SAVELOG ' + extra_output_value])
        marxan_writer.writerow(['OUTPUTDIR ' + setup_object.output_path])
        marxan_writer.writerow([])

        marxan_writer.writerow(['Program control.'])
        marxan_writer.writerow(['RUNMODE 1'])
        marxan_writer.writerow(['MISSLEVEL  ' + str(marxan_parameter_dict['missing_prop'])])
        marxan_writer.writerow(['ITIMPTYPE 0'])
        marxan_writer.writerow(['HEURTYPE -1'])
        marxan_writer.writerow(['CLUMPTYPE 0'])
        marxan_writer.writerow(['VERBOSITY 3'])
        marxan_writer.writerow([])


def marxan_update_setup_object(marxan_dialog, setup_object, marxan_parameter_dict):
    setup_object.output_name = marxan_parameter_dict['output_name']
    setup_object.num_iter = marxan_parameter_dict['num_iter']
    setup_object.num_runs = marxan_parameter_dict['num_run']
    setup_object.blm_value = marxan_parameter_dict['blm_value']
    setup_object.bound_flag = marxan_dialog.boundCheckBox.isChecked()
    setup_object.extra_outputs_flag = marxan_dialog.extraCheckBox.isChecked()
    setup_object.start_prop = marxan_parameter_dict['initial_prop']
    setup_object.target_prop = marxan_parameter_dict['missing_prop']

    return setup_object


def make_marxan_bat_file(setup_object):
    marxan_full_name = setup_object.marxan_path
    marxan_bat_file_name = marxan_full_name.replace('.exe', '.bat')
    if setup_object.analysis_type != 'MarxanWithZones':
        with open(marxan_bat_file_name, 'w', newline='', encoding='utf-8') as batFile:
            bat_writer = writer(batFile)
            bat_writer.writerow(['cd ' + path.dirname(marxan_full_name)])
            bat_writer.writerow([marxan_full_name])
    else:
        with open(marxan_bat_file_name, 'w', newline='', encoding='utf-8') as batFile:
            bat_writer = writer(batFile)
            bat_writer.writerow(['cd ' + path.dirname(marxan_full_name)])
            bat_writer.writerow(['\"' + marxan_full_name + '\"'])

    return marxan_bat_file_name


def waiting_for_marxan(setup_object, output_name):
    if setup_object.analysis_type != 'MarxanWithZones':
        marxan_path_name = setup_object.output_path + sep + output_name + '_best.txt'
    else:
        marxan_path_name = setup_object.output_path + sep + output_name + '_best.csv'
    try:
        while path.isfile(marxan_path_name) is False:
            sleep(2)
    except KeyboardInterrupt:
        pass


def add_best_marxan_output_to_pu_shapefile(setup_object, best_output_file_path, best_field_name):
    best_dict = make_best_scores_dict(best_output_file_path)
    pu_layer = QgsVectorLayer(setup_object.pu_path, "Planning units", "ogr")
    id_field_index = pu_layer.fields().indexFromName("Unit_ID")
    status_field_index = pu_layer.fields().indexFromName("Status")

    best_field_index = pu_layer.fields().indexFromName(best_field_name)
    provider = pu_layer.dataProvider()
    if best_field_index == -1:
        provider.addAttributes([QgsField(best_field_name, QVariant.String)])
        pu_layer.updateFields()
    best_field_index = provider.fieldNameIndex(best_field_name)

    progress_bar = make_progress_bar('Loading best output results')
    poly_total_count = pu_layer.featureCount()
    poly_count = 1

    pu_features = pu_layer.getFeatures()
    pu_layer.startEditing()
    for puFeature in pu_features:
        set_progress_bar_value(progress_bar, poly_count, poly_total_count)
        poly_count += 1

        pu_row = puFeature.id()
        pu_attributes = puFeature.attributes()
        pu_id = pu_attributes[id_field_index]
        pu_status = pu_attributes[status_field_index]
        best_bool = best_dict[pu_id]
        if pu_status == 'Conserved':
            best_status = 'Conserved'
        elif pu_status != 'Conserved' and best_bool == 1:
            best_status = 'Selected'
        else:
            best_status = '-'
        pu_layer.changeAttributeValue(pu_row, best_field_index, best_status)
    pu_layer.commitChanges()
    clear_progress_bar()


def make_best_scores_dict(best_output_file_path):
    best_scores_dict = dict()
    with open(best_output_file_path, 'rt') as f:
        best_output_reader = reader(f)
        next(best_output_reader, None)  # skip the headers
        for row in best_output_reader:
            pu_id = int(float(row[0]))
            best_bool = int(float(row[1]))
            best_scores_dict[pu_id] = best_bool

    return best_scores_dict


def add_summed_marxan_output_to_pu_shapefile(setup_object, summed_output_file_path, summed_field_name):
    summed_score_dict = make_summed_scores_dict(summed_output_file_path)

    pu_layer = QgsVectorLayer(setup_object.pu_path, 'Planning units', 'ogr')
    provider = pu_layer.dataProvider()
    id_field_index = provider.fieldNameIndex('Unit_ID')
    status_field_index = provider.fieldNameIndex('Status')

    summed_field_index = provider.fieldNameIndex(summed_field_name)
    if summed_field_index == -1:
        provider.addAttributes([QgsField(summed_field_name, QVariant.Int)])
        pu_layer.updateFields()
        summed_field_index = provider.fieldNameIndex(summed_field_name)

    progress_bar = make_progress_bar('Loading summed solution output results')
    poly_total_count = pu_layer.featureCount()
    poly_count = 1

    pu_features = pu_layer.getFeatures()
    pu_layer.startEditing()
    for puFeature in pu_features:
        set_progress_bar_value(progress_bar, poly_count, poly_total_count)
        poly_count += 1

        pu_row = puFeature.id()
        pu_attributes = puFeature.attributes()
        pu_id = pu_attributes[id_field_index]
        pu_status = pu_attributes[status_field_index]
        if pu_status == 'Conserved':
            summed_score = -99
        else:
            summed_score = summed_score_dict[pu_id]
        pu_layer.changeAttributeValue(pu_row, summed_field_index, summed_score)
    pu_layer.commitChanges()
    clear_progress_bar()


def make_summed_scores_dict(summed_output_file):
    summed_score_dict = dict()
    with open(summed_output_file, 'rt') as f:
        summed_output_reader = reader(f)
        next(summed_output_reader, None)  # skip the headers
        for row in summed_output_reader:
            pu_id = int(float(row[0]))
            summed_score = int(float(row[1]))
            summed_score_dict[pu_id] = summed_score

    return summed_score_dict


def make_calibrate_parameter_value_list(calibrate_raw_parameter_dict, exponential_bool):
    parameter_value_list = []
    num_analyses = float(calibrate_raw_parameter_dict['num_analyses_text'])
    orig_min_analyses = float(calibrate_raw_parameter_dict['min_analyses_text'])
    orig_max_analyses = float(calibrate_raw_parameter_dict['max_analyses_text'])

    if exponential_bool:
        if orig_min_analyses == 0:
            min_analyses = 0.00000000000000000000000000000001
        else:
            min_analyses = log(orig_min_analyses)
        max_analyses = log(orig_max_analyses)
    else:
        min_analyses = orig_min_analyses
        max_analyses = orig_max_analyses

    val_increase = (max_analyses - min_analyses) / (num_analyses - 1)

    for aValue in range(0, int(num_analyses)):
        parameter_value = float(min_analyses) + (val_increase * aValue)
        if exponential_bool:
            if orig_min_analyses == 0 and aValue == 0:
                parameter_value = 0
            else:
                parameter_value = exp(parameter_value)
        parameter_value_list.append(parameter_value)

    return parameter_value_list


def make_calibrate_results_dict(setup_object, marxan_parameter_dict):
    calibrate_results_dict = dict()
    score_list = list()
    cost_list = list()
    pu_count_list = list()
    connectivity_cost_list = list()
    penalty_list = list()
    mpm_list = list()

    summary_text_path = setup_object.output_path + sep + marxan_parameter_dict['output_name'] + '_sum.txt'
    if path.isfile(summary_text_path):
        with open(summary_text_path, 'rt') as f:
            summary_reader = reader(f)
            header_list = next(summary_reader)
            for aRow in summary_reader:
                score_value = float(aRow[header_list.index('Score')])
                cost_value = float(aRow[header_list.index('Cost')])
                pu_count_value = int(aRow[header_list.index('Planning_Units')])
                connectivity_cost_value = float(aRow[header_list.index('Connectivity')])
                penalty_value = float(aRow[header_list.index('Penalty')])
                mpm_value = float(aRow[header_list.index('MPM')])

                score_list.append(score_value)
                cost_list.append(cost_value)
                pu_count_list.append(pu_count_value)
                connectivity_cost_list.append(connectivity_cost_value)
                penalty_list.append(penalty_value)
                mpm_list.append(mpm_value)

        median_score = median(score_list)
        median_cost = median(cost_list)
        median_cost_pu_count = median(pu_count_list)
        median_connectivity = median(connectivity_cost_list)
        median_penalty = median(penalty_list)
        median_mpm = median(mpm_list)

        calibrate_results_dict['num_iter'] = marxan_parameter_dict['num_iter']
        calibrate_results_dict['num_run'] = marxan_parameter_dict['num_run']
        calibrate_results_dict['blm_value'] = marxan_parameter_dict['blm_value']
        calibrate_results_dict['spf_value'] = marxan_parameter_dict['spf_value']
        calibrate_results_dict['output_name'] = str(marxan_parameter_dict['output_name'])

        calibrate_results_dict['median_score'] = median_score
        calibrate_results_dict['median_cost'] = median_cost
        calibrate_results_dict['median_cost_pu_count'] = median_cost_pu_count
        calibrate_results_dict['median_connectivity'] = median_connectivity
        calibrate_results_dict['median_penalty'] = median_penalty
        calibrate_results_dict['median_mpm'] = median_mpm

    else:
        warning_message('No files found', 'The Marxan summary file was not found and so this process will terminate.')

    return calibrate_results_dict


def make_calibrate_spec_dat_file(setup_object, calibrate_spec_dat_file_name, spf_value):
    copy_target_dict = deepcopy(setup_object.target_dict)
    for featID in copy_target_dict:
        feat_list = copy_target_dict[featID]
        feat_list[2] = spf_value
        copy_target_dict[featID] = feat_list

    spec_dat_path_name = setup_object.input_path + sep + calibrate_spec_dat_file_name
    with open(spec_dat_path_name, 'w', newline='', encoding='utf-8') as out_file:
        spec_dat_writer = writer(out_file)
        spec_dat_writer.writerow(['id', 'name', 'target', 'spf', 'type'])

        feat_list = list(copy_target_dict.keys())
        feat_list.sort()

        for aFeat in feat_list:
            feat_list = copy_target_dict[aFeat]
            raw_feat_name = feat_list[0]
            change_bool, feat_name = convert_feat_name_by_changing_incompatible_text_characters(raw_feat_name)
            feat_target = feat_list[3]
            feat_spf = feat_list[2]
            feat_type = feat_list[1]
            spec_dat_writer.writerow([aFeat, feat_name, feat_target, feat_spf, feat_type])


def make_calibrate_output_file(result_path, calibrate_results_dict):
    with open(result_path, 'w', newline='', encoding='utf-8') as writerFile:
        calibrate_writer = writer(writerFile)
        header1 = ['Analysis', 'Name', 'Iterations', 'Runs', 'BLM', 'SPF']
        header2 = ['Med Portfolio Cost', 'Med Planning Unit cost', 'Med Boundary length', 'Med Feature Penalty cost', 'Med MPM', 'Med PU Count']
        final_header_row = header1 + header2
        calibrate_writer.writerow(final_header_row)

        analysis_number_list = list(calibrate_results_dict.keys())
        analysis_number_list.sort()
        for aNumber in analysis_number_list:
            analysis_dict = calibrate_results_dict[aNumber]

            num_iter = analysis_dict['num_iter']
            num_run = analysis_dict['num_run']
            blm_value = analysis_dict['blm_value']
            spf_value = analysis_dict['spf_value']
            output_name = analysis_dict['output_name']

            median_score = analysis_dict['median_score']
            median_cost = analysis_dict['median_cost']
            try:
                median_pu_count = analysis_dict['median_pu_count']
            except KeyError:
                median_pu_count = 'All PUs are selected in MwZ'  # for Marxan with Zones output
            median_connectivity = analysis_dict['median_connectivity']
            median_penalty = analysis_dict['median_penalty']
            median_mpm = analysis_dict['median_mpm']

            row_list1 = [str(aNumber + 1), output_name, str(num_iter), str(num_run), str(blm_value), str(spf_value)]
            row_list2 = [str(median_score), str(median_cost), str(median_connectivity), str(median_penalty), str(median_mpm), str(median_pu_count)]
            final_row_list = row_list1 + row_list2

            calibrate_writer.writerow(final_row_list)
