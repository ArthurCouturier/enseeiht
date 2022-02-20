import sudoku


class Solveur:
    def __init__(self):
        self._height = 0
        self._weight = 0
        self._sudoku = None
        self._possibilities = []

    def setSudoku(self, sudoku: sudoku.Sudoku):
        self._sudoku = sudoku.grille
        self._height = sudoku.height
        self._weight = sudoku.weight
        for h in range(self._height):
            self._possibilities.append([])
            for w in range(self._weight):
                self._possibilities[h].append([])

    def actualizePossibilities(self):
        changed = False
        for i in range(self._height):
            for j in range(self._weight):

                # For each case of the gride, we will test possibilities
                if int(self._sudoku[i][j]) == 0:
                    toCheck = [k+1 for k in range(max(self._height, self._weight))]
                    # Columns
                    for h in range(self._height):
                        if int(self._sudoku[h][j]) in toCheck:
                            toCheck.remove(int(self._sudoku[h][j]))
                    # Lines
                    for w in range(self._weight):
                        if int(self._sudoku[i][w]) in toCheck:
                            toCheck.remove(int(self._sudoku[i][w]))
                    # Squares
                    squarei = i//3
                    squarej = j//3
                    for h in range(3*squarei, 3 * (squarei + 1)):
                        for w in range(3 * squarej, 3 * (squarej + 1)):
                            if int(self._sudoku[h][w]) in toCheck:
                                toCheck.remove(int(self._sudoku[h][w]))

                    if len(toCheck) == 1:
                        new = []
                        for j2 in range(self._weight):
                            new.append(self._sudoku[i][j2] if not j2 == j else toCheck[0])
                        self._sudoku[i] = new
                        changed = True
                    elif len(toCheck) == 0:
                        print("exception in actualizePossibilities in solveur", i, j, self._sudoku[i][j], self.sudoku)
                        raise Exception("Pb in", self._sudoku[i][j], "can't find a solution")
                    else:
                        self._possibilities[i][j] = toCheck

        if not changed:
            # We will choose a number for the shortest possibilities list of the grid
            min, i, j = max(self._height, self._weight), 0, 0
            for h in range(self._height):
                for w in range(self._weight):
                    if int(self._sudoku[h][w]) == 0 and len(self._possibilities[h][w]) < min:
                        min, i, j = len(self._possibilities), h, w
            new = []
            for j2 in range(self._weight):
                new.append(self._sudoku[i][j2] if not j2 == j else self._possibilities[i][j][0])
            self._sudoku[i] = new

            while (not changed) and (i < self._height - 1 or j < self._height - 1):
                if (int(self._sudoku[i][j]) == 0) and (self._possibilities[i][j] != []):
                    new = []
                    for j2 in range(self._weight):
                        new.append(self._sudoku[i][j2] if not j2 == j else self._possibilities[i][j][0])
                    self._sudoku[i] = new
                    changed = True

                # Actualize parameters
                if j == self._height - 1:
                    i += 1
                    j = 0
                else:
                    j += 1

    def solve(self):
        finish = False
        iter = 1
        while (not finish):
            toCompare = []
            for i in range(len(self.sudoku)):
                toCompare.append(self.sudoku[i])
            # Create a list for each case with actual possible digits
            self.actualizePossibilities()

            # Check if the grid is completed
            completed = True
            for i in range(self._height):
                for j in range(self._weight):
                    if int(self._sudoku[i][j]) == 0:
                        completed = False

            if completed:
                print("All right I got it")
                finish = True
            if toCompare == self._sudoku:
                print("We can't go away")
                finish = True
            iter += 1
        print("iter = ", iter)


    @property
    def sudoku(self):
        return self._sudoku
