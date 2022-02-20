import csv
import numpy as np


class Sudoku:
    def __init__(self, grilleBrute, weight=9, height=9):
            self._weight = weight
            self._height = height
            self._grille = [grilleBrute[w * weight:(w + 1) * weight] for w in range(weight)]

    @property
    def weight(self):
        return self._weight

    @property
    def height(self):
        return self._height

    @property
    def grille(self):
        return self._grille
