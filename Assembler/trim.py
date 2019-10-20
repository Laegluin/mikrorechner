import re

def cut_comments(commands):
    return [re.sub(r'#(.)*','',command) for command in commands]

def cut_whitespace_lines(commands):
    return [command for command in commands if not re.match('\s*$',command)]

def trim_word_gap(commands):
    return [re.sub("[\s]", " ", command) for command in commands]

def cut_labels(commands):
    return [re.sub(r'\s+_[A-Za-z0-9_-]+\s*$','',command) for command in commands]

def trim_before_label_module(commands):
    trimmed_commands = commands
    trimmed_commands = cut_comments(trimmed_commands)
    trimmed_commands = cut_whitespace_lines(trimmed_commands)
    trimmed_commands = trim_word_gap(trimmed_commands)
    return trimmed_commands