# Сопоставьте название метода с принципом его работы
# append(object)`: Добавляет `object` в список
# count(value)`: Подсчитывает количество элементов со значением `value` в списке
# ndex(value)`: Находит первое слева вхождение `value` в список
# remove(value)`: Удаляет первое слева вхождение `value` в список
# sort()`: Сортирует список
# reverse()`: Переворачивает список





# Что будет выведено в результате выполнения приведённой ниже программы?   50
# data = [2, 4, 3, 5, 13, 11]
# data.append(12)
# print(sum(data))





# Что будет выведено в результате выполнения приведённой ниже программы?      None
# data = [2, 4, 3, 5, 13, 11]
# data.append(12)
# data = data.sort()
# print(data)





# В спорте N распределение мест на соревнованиях осуществляется следующим образом: участник делает 
# 7 попыток, каждая из которых  оценивается по 100-балльной шкале. Из них отбираются четыре лучшие, и их сумма выводится на экран. По заданным оценкам определите итоговый результат участника.
# Входные данные
# На вход подаются результаты участника за каждую попытку. Каждое число с новой строки.
# Выходные данные
# Программа должна вывести одно число — сумму баллов за четыре лучшие попытки участника.
# Sample Input:
# 10
# 50
# 70
# 28
# 43
# 87
# 2
# Sample Output:
# 250
# A = []
# for _ in range(7):
#     A.append(int(input()))
# A.sort(reverse=True)
# t = sum(A[:4]) 
# print(t)




# Школьный учитель на классном часе попросил каждого ученика назвать одно произвольное целое число. Необходимо проверить, можно ли из полученных чисел составить арифметическую прогрессию.
# Входные данные
# На вход подаётся число n, а затем n целых чисел, каждое с новой строки.
# Выходные данные
# Программа должна вывести «YES», если из названных чисел можно составить арифметическую прогрессию и «NO» в противном случае. 
# Sample Input 1:
# 5
# 7
# 4
# 13
# 1
# 10
# Sample Output 1:
# YES
# Sample Input 2:
# 4
# 100
# 1
# 54
# 12
# Sample Output 2:
# NO
# n = int(input()) 
# D = []
# for i in range(n):
#     a = int(input())  
#     D.append(a)
# D.sort()  
# if n < 2:
#     print("YES")  
# else:
#     c = D[1] - D[0]  
#     igh = True
#     for i in range(1, n - 1):
#         if D[i + 1] - D[i] != c:
#             igh = False
#             break
#     if igh:
#         print("YES")
#     else:
#         print("NO")







# Для покупки билетов на концерт популярного исполнителя Ковы Клаки требовалось заполнить небольшую анкету, в которой, помимо прочего, необходимо было указать свой возраст. Кове стало интересно, в каком возрасте количество её фанатов максимально, но так как в математике и программировании она разбирается намного хуже, чем в вокальном искусстве, было принято решение привлечь вас для решения данной задачи. За это вам обещали халявный билетик!
# Входные данные
# На вход подаётся количество купленных билетов n, а затем n целых чисел, обозначающих возраст каждого посетителя концерта. Каждое число вводится с новой строки.
# Выходные данные
# Программа должна вывести одно число — самый часто встречающийся возраст на концерте. Если таких возрастов несколько, то укажите самый маленький из них.
# Sample Input:
# 3
# 10
# 20
# 10
# Sample Output:
# 10
# 00000000
# n = int(input())  
# ages = []        
# for _ in range(n):
#     age = int(input())
#     ages.append(age)
# m_age = max(ages)
# c = [0] * (m_age + 1)  
# for age in ages:
#     c[age] += 1
# m_count = 0
# r_age = 0
# for age in range(len(c)):
#     if c[age] > m_count:
#         m_count = c[age]
#         r_age = age
#     elif c[age] == m_count and age < r_age:
#         r_age = age
# print(r_age)



# Методы split() и join() применяются   Только к строкам




# Что будет выведено в результате выполнения приведённой ниже программы при вводе строки 10 11 0 -4 5?  11010*11*0*-4*5
# data = input().split()
# print(data[1] + data[2] + '*'.join(data))
# Если программа завершится с ошибкой, то в ответе укажите «Ошибка».





# Что будет выведено в результате выполнения приведённой ниже программы?  «Ошибка»
# a = [10, 15, 3]
# a.remove(15)
# a.append(11)
# print(''.join(a))
# Если программа завершится с ошибкой, то в ответе укажите «Ошибка».






# Димаста — репетитор по математике с большим количеством учеников. После ЕГЭ он собрал результаты своих подопечных и решил посчитать их средний балл. Для большей достоверности Димаста решил удалить один самый высокий и один самый низкий результат своих учеников. Напишите программу, которая считает необходимое Димасте значение.
# Входные данные
# На вход подаётся одна строка — результаты учеников Димасты, разделённые пробелами. Гарантируется, что значений больше двух.
# Выходные данные
# Программа должна вывести одно число — среднее арифметическое баллов учеников репетитора, исключая одного ученика с самым высоким и одного с самым низким баллами.
# Sample Input 1:
# 45 77 93 66
# Sample Output 1:
# 71.5
# Sample Input 2:
# 90 90 0
# Sample Output 2:
# 90.0
# a = input()
# b = a.split()
# for i in range(len(b)):
#     b[i] = int(b[i]) 
# p = max(b)
# g = min(b)
# b.remove(p)
# b.remove(g)
# f = sum(b) / len(b)
# print(f)









# Спустя месяц Михаил и Жанна решили изменить подход: теперь за все покупки платит Михаил, если количество чётных стоимостей превышает количество нечётных; Жанна, если количество нечётных превышает количество чётных; и супруги платят поровну, если чётных и нечётных стоимостей одинаковое количество.
# Входные данные
# На вход подаётся перечень стоимостей покупок, все цены разделены пробелами.
# Выходные данные
# Программа должна вывести, какой из супругов оплачивал покупки и какая сумма при это была заплачена.
# Sample Input 1:
# 10 20 50 43
# Sample Output 1:
# Михаил 123
# Sample Input 2:
# 11 17 24 101
# Sample Output 2:
# Жанна 153
# Sample Input 3:
# 30 15 50 85
# Sample Output 3:
# Поровну 180
# a = input()
# b = a.split()
# for i in range(len(b)): 
#     b[i] = int(b[i]) 
# f = 0  
# t = 0 
# for i in b:
#     if i % 2 == 0:
#         f += 1
#     else:
#         t += 1
# to = sum(b)  
# if f > t:
#     print("Михаил", to)
# elif t > f:
#     print("Жанна", to)
# else:
#     print("Поровну", to)








# В библиотеке установили терминалы, которые позволяют посетителям понять, есть ли сейчас в наличии та или иная книга. Данные в этом терминале обновляются в режиме реального времени, данные хранятся в специальном списке. Вам лишь нужно написать программу, которая определяет, если ли книга в наличии, и если есть, то выводит индекс нахождения этой книги в списке.
# Входные данные
# На вход подаются две строки, в первой — названия книг, которые есть в наличии. Все названия разделены пробелами. Гарантируется, что пробелов в названии одной книги нет. Во второй — книга, которую нужно найти.
# Выходные данные
# Если книга присутствует в списке, то выведите её индекс первого вхождения. Учтите, что одинаковых книг может быть несколько.
# Если книга не найден в списке, то вывести −1.
# Sample Input 1:
# 451_градус_по_Фаренгейту Триумфальная_арка 1984 Война_и_мир Маленький_принц Война_и_мир
# Война_и_мир
# Sample Output 1:
# 3
# Sample Input 2:
# 451_градус_по_Фаренгейту Триумфальная_арка 1984 Война_и_мир Маленький_принц Война_и_мир
# Собачье_сердце
# Sample Output 2:
# -1
# a = input()
# b = a.split()
# f = input()
# index = -1  
# for i in range(len(b)):
#     if b[i] == f:
#         index = i  
#         break      
# print(index)









# Екатерина — филолог по образованию. Темой её курсовой работы стал анализ самых длинных слов выбранного произведения. Разумеется, находить это вручную не представляется возможным, поэтому ей необходимо написать программу, которая поможет с данной задачей. И снова вас призвали на помощь!
# Входные данные
# На вход подаётся одна строка — текст произведения. В тексте слова разделены пробелами. Также в тексте могут встретиться знаки препинания из набора «,.?!:;».
# Выходные данные
# Программа должна вывести одну строку — самое длинное слово в произведении. Если таких слов несколько, выведите то, которое встретилось в тексте раньше.
# Sample Input:
# В лесу родилась ёлочка, в лесу она росла
# Sample Output:
# родилась
# a = input()
# f = ".,?!:;"
# for i in f:
#     a = a.replace(i, '')
# b = a.split()
# l_w = ""
# for w in b:
#     if len(w) > len(l_w):
#         l_w = w
# print(l_w)



