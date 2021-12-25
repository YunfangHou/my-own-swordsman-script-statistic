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


# 角色对话量，对于连续的四段台词 A B C D，如果 A C 为同一角色，B D 为同一角色，则视作一次对话
def character_communication(character_name1, character_name2):
    # lines 为实际对话段数
    communication_lines = 0
    # index 为交流指数，连续的对话可以呈叠加效果，迅速提升交流指数
    communication_index = 0
    with open('script.txt') as temp_f:
        datafile = temp_f.readlines()
    name_list = [character_name1, character_name2]
    successive_line = 0
    successive_communication = 0
    for line in datafile:
        if not line.isspace():
            name = name_list[successive_line % 2]
            if line[0:len(name) + 1] == name + '：':
                successive_line += 1
                if successive_communication >= 0:
                    communication_index += successive_communication
                    if successive_line % 2 == 0:
                        communication_lines += 2
                if successive_line == 4:
                    successive_line = 0
                    communication_index += 1
                    successive_communication += 1
            else:
                successive_line = 0
                successive_communication = 0
        else:
            continue

    communication_lines = (int)((communication_lines + 1) / 2)
    return [communication_lines, communication_index]


def write_character_communication(character_name1, character_name2, file_name):
    communication = character_communication(character_name1, character_name2)[0]
    index = character_communication(character_name1, character_name2)[1]

    row = (character_name1, character_name2, communication, index)
    with open(file_name, 'a') as f:
        writer = csv.writer(f)
        writer.writerow(row)
        f.close()


def whole_test_of_communication():
    headers = ['角色一', '角色二', '交流台词段数', '互动指数']
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
        sub_list = main_character_list[name_index + 1:len(main_character_list)]
        for responder in sub_list:
            write_character_communication(name, responder, 'communication_statistic.csv')
        name_index += 1


def single_character_communication_with_main_character(name):
    headers = ['角色一', '角色二', '交流台词段数', '互动指数']
    file_name = name + '_communication.csv'
    with open(file_name, 'w') as f:
        writer = csv.writer(f)
        writer.writerow(headers)
        f.close()
    for main_character_name in ['邢育森', '佟湘玉', '白展堂', '吕秀才', '莫小贝', '郭芙蓉', '李大嘴', '燕小六', '祝无双', '杨蕙兰', '柳星雨']:
        write_character_communication(name, main_character_name, file_name)


if __name__ == '__main__':
    # headers = ['角色名', '全剧台词段数', '全剧台词字数', '段均字数']
    # with open('lines_statistic.csv', 'w') as f:
    #     writer = csv.writer(f)
    #     writer.writerow(headers)
    #     f.close()
    #
    # for name in find_all_character_name():
    #     write_character_statistic(name, 'lines_statistic.csv')

    # whole_test_of_communication()
    single_character_communication_with_main_character('李大嘴')
