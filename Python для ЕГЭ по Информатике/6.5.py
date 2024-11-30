# Иннокентий составляет 
# 5-буквенные слова, в которых есть только буквы А, Р, Б, У, З, И, К. Причём в каждом слове содержится не более 
# 3 гласных. Каждая буква может входить в слово несколько раз или не встречаться совсем. Словом считается любая допустимая последовательность букв, не обязательно осмысленная. Сколько существует таких слов, которые может написать Иннокентий?
# k = 0
# for a1 in 'АРБУЗИК':
#     for a2 in 'АРБУЗИК':
#         for a3 in 'АРБУЗИК':
#             for a4 in 'АРБУЗИК':
#                 for a5 in 'АРБУЗИК':
#                     w = a1 + a2 +  a3 + a4+ a5
#                     if w.count("А") + w.count("И") +w.count("У") <= 3:
#                         k += 1
# print(k)






# Алёша составляет 6-буквенные слова, в которых есть только буквы Ш, К, О, Л, А. Каждая буква может входить в слово несколько раз или не встречаться совсем, но не должно быть двух гласных, идущих подряд. Словом считается любая допустимая последовательность букв, не обязательно осмысленная. Сколько существует таких слов, которые может написать Алёша?
# k = 0
# for a1 in 'ШКОЛА':
#     for a2 in 'ШКОЛА':
#         for a3 in 'ШКОЛА':
#             for a4 in 'ШКОЛА':
#                 for a5 in 'ШКОЛА':
#                     for a6 in 'ШКОЛА':
#                         w = a1 + a2 + a3 + a4 + a5 + a6
#                         if ('ОА' not in w) and ('АО' not in w) and ('АА' not in w) and ("ОО" not in w):
#                             k +=1
# print(k)





# Сколько существует восьмеричных четырёхзначных чисел, в которых все цифры различны и цифра «2» не стоит рядом с цифрой «6»?
# k = 0
# for a1 in '1234567':
#     for a2 in '01234567':
#         for a3 in '01234567':
#             for a4 in '01234567':
#                 his = a1 + a2+ a3 +a4
#                 if (a1!=a2 and a2!=a3 and a3!=a4 and a1!=a3 and a1!=a4 and a2!=a4) and '26' not in his and '62' not in his:
#                     k +=1
# print(k)




# Дионис составляет 5-буквенные слова из букв своего имени. Каждая буква может входить в слово любое количество раз или не встречаться совсем, но в слове не должно быть согласных букв больше, чем гласных. Словом считается любая допустимая последовательность букв, не обязательно осмысленная. Сколько существует таких слов, которые может написать Дионис?
# k = 0
# for a1 in 'DIONS':
#     for a2 in 'DIONS':
#         for a3 in 'DIONS':
#             for a4 in 'DIONS':
#                 for a5 in 'DIONS':
#                     w = a1 + a2 + a3 +a4 +a5
#                     if w.count('D')+w.count('N')+w.count('S')<=w.count('O')+w.count('I'):
#                         k +=1 
# print(k)




# Исполнитель Редактор получает на вход строку цифр и преобразовывает её. Редактор может выполнять две команды, в обеих командах v и w обозначают цепочки цифр.
# А) заменить (v, w).
# Эта команда заменяет в строке первое слева вхождение цепочки v на цепочку w. Например, выполнение команды заменить (111, 27) преобразует строку 05111150 в строку 0527150. Если в строке нет вхождений цепочки v, то выполнение команды заменить (v, w) не меняет эту строку.
# Б) нашлось (v).
# Эта команда проверяет, встречается ли цепочка v в строке исполнителя Редактор. Если она встречается, то команда возвращает логическое значение «истина», в противном случае возвращает значение «ложь». Строка исполнителя при этом не изменяется.
# Какая строка получится в результате применения приведённой ниже программы к строке, состоящей из 
# 100 идущих подряд цифр 3? В ответе запишите полученную строку.
# НАЧАЛО
#   ПОКА нашлось (333) ИЛИ нашлось (1111)
#     ЕСЛИ нашлось (333)
#       ТО заменить (333, 1)
#       ИНАЧЕ заменить (1111, 3)
#     КОНЕЦ ЕСЛИ
#   КОНЕЦ ПОКА
# КОНЕЦ
# v = '3' * 100
# while '333' in v or '1111' in v:
#     if '333' in v:
#         v = v.replace('333' , '1', 1)
#     else:
#         v = v.replace('1111', '3',1)
# print(v)






# Исполнитель Редактор получает на вход строку цифр и преобразовывает её. Редактор может выполнять две команды, в обеих командах v и w обозначают цепочки цифр.
# А) заменить (v, w).
# Эта команда заменяет в строке первое слева вхождение цепочки v на цепочку w. Например, выполнение команды
# заменить (111, 27) преобразует строку 05111150 в строку 0527150. Если в строке нет вхождений цепочки v, то выполнение команды заменить (v, w) не меняет эту строку.
# Б) нашлось (v).
# Эта команда проверяет, встречается ли цепочка v в строке исполнителя Редактор. Если она встречается, то команда возвращает логическое значение «истина», в противном случае возвращает значение «ложь». Строка исполнителя при этом не изменяется.
# Дана программа для Редактора:
# НАЧАЛО
# ПОКА нашлось (22) ИЛИ нашлось (333) ИЛИ нашлось (9999)
#   ЕСЛИ нашлось (22)
#     ТО заменить (22, 3)
#   КОНЕЦ ЕСЛИ
#   ЕСЛИ нашлось (333)
#     ТО заменить (333, 99)
#   КОНЕЦ ЕСЛИ
#   ЕСЛИ нашлось (9999)
#     ТО заменить (9999, 22)
#   КОНЕЦ ЕСЛИ
# КОНЕЦ ПОКА
# КОНЕЦ
# На вход приведённой выше программе поступает строка, начинающаяся с цифры «9», а затем содержащая и n цифр «3<n<1000).
# Определите наибольшее возможное значение произведения числовых значений цифр в строке, которая может быть результатом выполнения программы.
# m = 0 
# for i in range(4, 1000):
#     v = '9' + '3' * i  
#     while '22' in v or '333' in v or '9999' in v:
#         if '22' in v:
#             v = v.replace('22', '3', 1)
#         if '333' in v:
#             v = v.replace('333', '99', 1)
#         if '9999' in v:
#             v = v.replace('9999', '22', 1)
#     product = 1
#     for g in v:
#         product *= int(g)
#     if product > m:
#         m = product
# print(m)





# Исполнитель Редактор получает на вход строку цифр и преобразовывает её. Редактор может выполнять две команды, в обеих командах v и w обозначают цепочки цифр.
# А) заменить (v, w).
# Эта команда заменяет в строке первое слева вхождение цепочки v на цепочку w. Например, выполнение команды
# заменить (111, 27) преобразует строку 05111150 в строку 0527150. Если в строке нет вхождений цепочки v, то выполнение команды заменить (v, w) не меняет эту строку.
# Б) нашлось (v).
# Эта команда проверяет, встречается ли цепочка v в строке исполнителя Редактор. Если она встречается, то команда возвращает логическое значение «истина», в противном случае возвращает значение «ложь». Строка исполнителя при этом не изменяется.
# Дана программа для Редактора:
# НАЧАЛО
# ПОКА нашлось (575) ИЛИ нашлось (7777)
#   ЕСЛИ нашлось (575)
#     ТО заменить (575, 77)
#   КОНЕЦ ЕСЛИ
#   ЕСЛИ нашлось (7777)
#     ТО заменить (7777, 5)
#   КОНЕЦ ЕСЛИ
# КОНЕЦ ПОКА
# КОНЕЦ
# На вход приведённой выше программе поступает строка, состоящая из n подряд идущих групп «5<n<1000).
# Определите максимальную длину строки, которая могла получиться в процессе выполнения программы и укажите её в качестве ответа.
# c = 0
# for i in range(6 ,1000):
#     v = '57' *i
#     while '575'in v or '7777' in v:
#         if '575' in v:
#             v = v.replace('575', '77', 1)
#         if '7777' in v:
#             v = v.replace('7777', '5' , 1)
#     b = 0
#     v = int(v)
#     while v != 0:
#         b += 1
#         v //= 10
#     if b > c:
#         c = b
# print (c)