import csv


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


def write_character_statistic(character_name, file_name):
    lines = count_character_lines(character_name)
    words = count_character_word(character_name)

    if lines > 100:
        row = (character_name, lines, words, words / lines)
        with open(file_name, 'a') as f:
            writer = csv.writer(f)
            writer.writerow(row)
            f.close()


# 角色对话量，对于连续的四段台词 A B C D，如果 A C 为同一角色，B D 为同一角色，则增加对话量指数
def character_communication(character_name1, character_name2):
    communication_amount = 0
    with open('script.txt') as temp_f:
        datafile = temp_f.readlines()
    name_list = [character_name1, character_name2]
    successive_line = 0
    for line in datafile:
        if not line.isspace():
            name = name_list[successive_line % 2]
            if line[0:len(name) + 1] == name + '：':
                successive_line += 1
                if successive_line == 4:
                    successive_line = 0
                    communication_amount += 1
            else:
                successive_line = 0
        else:
            continue

    return communication_amount


def write_character_communication(character_name1, character_name2, file_name):
    communication = character_communication(character_name1, character_name2)
    row = (character_name1, character_name2, communication)
    with open(file_name, 'a') as f:
        writer = csv.writer(f)
        writer.writerow(row)
        f.close()


if __name__ == '__main__':
    # headers = ['角色名', '全剧台词段数', '全剧台词字数', '段均字数']
    # with open('lines_statistic.csv', 'w') as f:
    #     writer = csv.writer(f)
    #     writer.writerow(headers)
    #     f.close()
    #
    # for name in find_all_character_name():
    #     write_character_statistic(name, 'lines_statistic.csv')

    headers = ['交流发起者', '交流应答者', '交流量']
    with open('communication_statistic.csv', 'w') as f:
        writer = csv.writer(f)
        writer.writerow(headers)
        f.close()

    main_character_list = []
    for name in find_all_character_name():
        lines_amount = count_character_lines(name)
        if lines_amount > 2000:
            main_character_list.append(name)
            print(name)

    name_index = 0
    for name in main_character_list:
        sub_list = main_character_list[name_index+1:len(main_character_list)]
        for responder in sub_list:
            write_character_communication(name, responder, 'communication_statistic.csv')
        name_index += 1