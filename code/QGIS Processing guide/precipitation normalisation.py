"""
Model exported as python.
Name : Precipitation Normalisation
Group : 
With QGIS : 31608
"""

from qgis.core import QgsProcessing
from qgis.core import QgsProcessingAlgorithm
from qgis.core import QgsProcessingMultiStepFeedback
from qgis.core import QgsProcessingParameterRasterLayer
from qgis.core import QgsProcessingParameterRasterDestination
from qgis.core import QgsProcessingParameterBoolean
import processing


class PrecipitationNormalisation(QgsProcessingAlgorithm):

    def initAlgorithm(self, config=None):
        self.addParameter(QgsProcessingParameterRasterLayer('inputraster', 'input raster', defaultValue=None))
        self.addParameter(QgsProcessingParameterRasterDestination('Outpu_raster', 'outpu_raster', createByDefault=True, defaultValue=None))
        self.addParameter(QgsProcessingParameterBoolean('VERBOSE_LOG', 'Verbose logging', optional=True, defaultValue=False))

    def processAlgorithm(self, parameters, context, model_feedback):
        # Use a multi-step feedback, so that individual child algorithm progress reports are adjusted for the
        # overall progress through the model
        feedback = QgsProcessingMultiStepFeedback(2, model_feedback)
        results = {}
        outputs = {}

        # Raster calculator
        alg_params = {
            'FNAME': False,
            'FORMULA': 'ifelse(a>350, a=1, a/350)',
            'GRIDS': parameters['inputraster'],
            'NAME': 'Calculation',
            'RESAMPLING': 0,
            'TYPE': 7,
            'USE_NODATA': False,
            'XGRIDS': None,
            'RESULT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['RasterCalculator'] = processing.run('saga:rastercalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(1)
        if feedback.isCanceled():
            return {}

        # Translate (convert format)
        alg_params = {
            'COPY_SUBDATASETS': False,
            'DATA_TYPE': 0,
            'EXTRA': '',
            'INPUT': outputs['RasterCalculator']['RESULT'],
            'NODATA': None,
            'OPTIONS': '',
            'TARGET_CRS': 'ProjectCrs',
            'OUTPUT': parameters['Outpu_raster']
        }
        outputs['TranslateConvertFormat'] = processing.run('gdal:translate', alg_params, context=context, feedback=feedback, is_child_algorithm=True)
        results['Outpu_raster'] = outputs['TranslateConvertFormat']['OUTPUT']
        return results

    def name(self):
        return 'Precipitation Normalisation'

    def displayName(self):
        return 'Precipitation Normalisation'

    def group(self):
        return ''

    def groupId(self):
        return ''

    def createInstance(self):
        return PrecipitationNormalisation()
