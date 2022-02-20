import sudoku


class Verifieur:
    def __init__(self):
        self._height = 0
        self._weight = 0
        self._sudoku = None
        self._success = True

    @property
    def success(self):
        return self._success

    def setSudoku(self, sudoku: sudoku.Sudoku):
        self._sudoku = sudoku.grille
        self._weight = sudoku.weight
        self._height = sudoku.height

    def setSudokuWithList(self, l: list):
        self._sudoku = l

    def isInList(self, l, nb):
        for i in range(len(l)):
            if int(nb) == l[i]:
                return True
        return False

    @DeprecationWarning
    def check(self, range1, range2, inv=False):
        for v1 in range(range1):
            list = [i for i in range(1, range2 + 1)]
            for v2 in range(range2):
                if not self.isInList(list, self._sudoku[v1 if inv else v2][v2 if inv else v1]):
                    self._success = False
                else:
                    list.remove(int(self._sudoku[v1 if inv else v2][v2 if inv else v1]))
            if not list == []:
                self._success = False

    def fusion(self, left, right):
        res, il, ir = [], 0, 0
        while il < len(left) and ir < len(right):
            if left[il] <= right[ir]:
                res.append(left[il])
                il += 1
            else:
                res.append(right[ir])
                ir += 1
        if left:
            res.extend(left[il:])
        if right:
            res.extend(right[ir:])
        return res

    def fusionSort(self, v):
        if len(v) <= 1:
            return v
        mid = len(v)//2
        if mid != 0:
            left = self.fusionSort(v[:mid])
            right = self.fusionSort(v[mid:])
        return self.fusion(list(left), list(right))

    def check2(self, toSort, objective):
        sorted = self.fusionSort(toSort)
        for k in range(len(objective)):
            if not int(sorted[k]) == objective[k]:
                return False
        return True

    def solve(self):
        # Vérifier les colonnes en parcourant donc sur la taille d'une ligne
        for k in range(self._height):
            toCheck = []
            for k2 in range(self._weight):
                toCheck.append(self._sudoku[k2][k])
            if not self.check2(toCheck, [i+1 for i in range(self._height)]):
                self._success = False
                print("False because of column problem: column number", k)

        # Vérifier les lignes
        for k in range(self._weight):

            if not self.check2(self._sudoku[k], [i+1 for i in range(self._weight)]):
                self._success = False
                print("False because of line problem: line number", k)

        # Vérifier les carrés
        for k1 in range(3):
            for k2 in range(3):
                toCheck = []
                for k11 in range(3*k1, 3*(k1+1)):
                    for k22 in range(3*k2, 3*(k2+1)):
                        toCheck.append(self._sudoku[k11][k22])
                if not self.check2(toCheck, [i+1 for i in range(self._weight)]):
                    self._success = False
                    print("False because of square problem: square number", k1, k2)
