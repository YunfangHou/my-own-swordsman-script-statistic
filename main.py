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


def write_character_statistic(character_name):
    lines = count_character_lines(character_name)
    words = count_character_word(character_name)

    if lines > 100:
        row = (character_name, lines, words, words/lines)
        with open('statistic.csv', 'a') as f:
            writer = csv.writer(f)
            writer.writerow(row)
            f.close()


if __name__ == '__main__':
    headers = ['角色名', '全剧台词段数', '全剧台词字数', '段均字数']
    with open('statistic.csv', 'w') as f:
        writer = csv.writer(f)
        writer.writerow(headers)
        f.close()

    for name in find_all_character_name():
        write_character_statistic(name)

