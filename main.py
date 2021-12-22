import importlib
import sys
import time


def count_character_lines(character_name):
    lines_amount = 0
    with open('script.txt') as temp_f:
        datafile = temp_f.readlines()
    for line in datafile:
        if character_name + '：' in line:
            lines_amount += 1
    return lines_amount


def count_character_word(character_name):
    word_amount = 0
    with open('script.txt') as temp_f:
        datafile = temp_f.readlines()
    for line in datafile:
        if character_name + '：' in line:
            word_amount += len(line) - len(character_name) - 1
    return word_amount


def find_all_character_name():
    character_name_list = []
    with open('script.txt') as temp_f:
        datafile = temp_f.readlines()
    for line in datafile:
        try:
            for i in [1, 2, 3, 4, 5]:
                if line[i] == '：':
                    character_name = line[0:i]
                    if character_name_list.count(character_name) == 0:
                        character_name_list.append(character_name)
        except BaseException:
            continue

    return character_name_list


def show_character_statistic(character_name):
    lines = count_character_lines(character_name)
    words = count_character_word(character_name)

    if lines > 100:
        print(character_name
              + ' 的全剧台词段数为： ' + str(lines)
              + '    ，全剧台词字数为： ' + str(words)
              + '    ，段均字数为：' + str(words/lines))


if __name__ == '__main__':
    # for name in find_all_character_name():
    #     show_character_statistic(name)
    print('test')

