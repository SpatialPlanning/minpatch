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

from PyQt5.QtWidgets import QMessageBox
from PyQt5.QtWidgets import QProgressBar
from PyQt5.QtCore import Qt

from qgis.core import Qgis
from qgis.utils import iface


###################### WHAT ABOUT CRITICAL?


def info_message(title_text, main_text):
    iface.messageBar().pushMessage(title_text, main_text, level=Qgis.Info)


def warning_message(title_text, main_text):
    iface.messageBar().pushMessage(title_text, main_text, level=Qgis.Warning, duration=0)


def critical_message(title_text, main_text):
    iface.messageBar().pushMessage(title_text, main_text, level=Qgis.Critical, duration=0)


def success_message(title_text, main_text):
    iface.messageBar().pushMessage(title_text, main_text, level=Qgis.Success)


def make_progress_bar(progress_text):
    iface.messageBar().clearWidgets()
    progress_message_bar = iface.messageBar()
    progress_bar = QProgressBar()
    progress_bar.setMaximum(100)
    progress_bar.setTextVisible(True)
    progress_bar.setFormat(progress_text)
    progress_bar.setAlignment(Qt.AlignCenter)
    progress_message_bar.pushWidget(progress_bar)

    return progress_bar


def set_progress_bar_value(progress_bar, numerator_value, denominator_value):
    progress_bar_value = int((numerator_value / denominator_value) * 100)
    progress_bar.setValue(progress_bar_value)


def set_progress_bar_value_complicated(progress_bar, numerator_value, denominator_value, multiplier, add_on):
    progress_bar_value = int(add_on + (numerator_value / denominator_value) * multiplier)
    progress_bar.setValue(progress_bar_value)


def clear_progress_bar():
    iface.messageBar().clearWidgets()


def empty_polygon_pu_id_set_error_message(empty_polygon_pu_id_set):
    empty_polygon_pu_id_list = list(empty_polygon_pu_id_set)
    empty_polygon_pu_id_list.sort()
    pu_id_string = ''
    for pu_id in empty_polygon_pu_id_list:
        pu_id_string += str(pu_id) + ', '
    final_pu_id_string = pu_id_string[0:-2]
    warning_message("Shapefile error", "Planning units with the following ID values have problems with their topology and could not be processed by QGIS: " + final_pu_id_string)


def check_change_earmarked_to_available_pu():
    warning_title_text = 'Confirm changes to planning unit status'
    warning_main_text = 'This will change the status of the Earmarked planning units to Available. Do you want to continue?'
    warning_bool = run_yes_cancel_warning_dialog_box(warning_title_text, warning_main_text)
    return warning_bool


def zones_check_change_earmarked_to_available_pu():
    warning_title_text = 'Confirm changes to planning unit status'
    warning_main_text = 'This will change the status of the Earmarked planning units to Unassigned. Do you want to continue?'
    warning_bool = run_yes_cancel_warning_dialog_box(warning_title_text, warning_main_text)
    return warning_bool


def run_yes_cancel_warning_dialog_box(title_text, main_text):
    answer = QMessageBox.warning(None, title_text, main_text, QMessageBox.Yes | QMessageBox.Cancel)
    if answer == QMessageBox.Yes:
        warning_bool = True
    else:
        warning_bool = False

    return warning_bool



