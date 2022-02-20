# @Author: Arthur Couturier
import solveur
import sudoku
import csv
import verifieur


number = 5 # Number of the sudoku you want to check in your database
height = 9
weight = 9


def toSudoku(l):
    finalRes = []
    for i in range(len(l)):
        res = ''
        for j in range(len(l[0])):
            res += str(l[i][j])
        finalRes.append(res)
    return finalRes

with open("sudoku.csv", newline='') as file:
    reader = csv.reader(file)
    header = next(reader)
    if number > 1:
        for i in range(number - 1):
            next(reader)
    quizzBrut, solutionBrute = next(reader)
    quizz = sudoku.Sudoku(quizzBrut)
    solution = sudoku.Sudoku(solutionBrute)

print(quizz.grille)
print(solution.grille)
verif = verifieur.Verifieur()
verif.setSudoku(solution)
verif.solve()
print(verif.success)

solve = solveur.Solveur()
solve.setSudoku(quizz)
solve.solve()
print("after solver", solve.sudoku)
print("quizz", quizz.grille)

outputSolv = toSudoku(solve.sudoku)
verif.setSudokuWithList(outputSolv)
verif.solve()
print("verif after solv", verif.success)
quizz = sudoku.Sudoku(quizzBrut)
print("the     quizz", quizz.grille)
print("your solution", toSudoku(solve.sudoku))
print("the real solu", solution.grille)
