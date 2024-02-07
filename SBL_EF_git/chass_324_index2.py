#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  7 13:40:10 2024

@author: ali
"""
import nibabel as nib
import numpy as np
from scipy.ndimage import morphology
from nibabel import load, save, Nifti1Image, squeeze_image
import os
import sys, string, os
import pandas as pd
import openpyxl

path ='/Volumes/dusom_mousebrains/All_Staff/Nariman_fmri_pipeline/chass_symmetric3/chass_symmetric3_labels_PLI_res.nii.gz'
nii=nib.load(path)
nii.shape
data =nii.get_fdata()
roi=np.unique(data)
roi = roi[1:]

