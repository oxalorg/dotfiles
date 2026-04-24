echo "$*" | iconv -t ascii//TRANSLIT | sed -E 's/[^a-zA-Z0-9]+/-/g' | sed -E 's/^-+\|-+$//g' | tr A-Z a-z

# for f in *.{txt,md}; do
#     birth_time=$(stat -t "%y-%m-%d" "$f" | awk '{print $12}' | cut -d '"' -f 2)
#     filename="${f%.*}"
#     echo "Moving $f to $birth_time-$slug.txt"
#     mv "$f" "$birth_time-$slug.txt"
# done
