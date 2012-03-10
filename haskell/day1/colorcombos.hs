colors = ["black", "white", "blue", "yellow", "red"]

all_uniq_ordered_pairs l = [(x,y) | x <- l, y <- l, x < y]

