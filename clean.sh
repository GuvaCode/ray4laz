#!/bin/bash

# clean.sh - Скрипт для очистки проекта

echo "Начинаем очистку проекта..."

# 1. В папке binary удаляем все файлы, кроме папки resources
if [ -d "binary" ]; then
    echo "Очистка binary (сохраняем папку resources)..."
    find binary -mindepth 1 -maxdepth 1 ! -name "resources" -exec rm -rf {} +
    echo "✓ binary очищена"
else
    echo "⚠ Папка binary не найдена"
fi

# 2. Удаляем все папки с именем backup во всех указанных директориях
#    (examples, gui_style, headers, libs, MSEIde, source, tool)
dirs_with_backup=("examples" "gui_style" "headers" "libs" "MSEIde" "source" "tool")

for dir in "${dirs_with_backup[@]}"; do
    if [ -d "$dir" ]; then
        echo "Удаление папок backup в $dir..."
        find "$dir" -type d -name "backup" -exec rm -rf {} + 2>/dev/null
        echo "✓ $dir обработана"
    else
        echo "⚠ Папка $dir не найдена"
    fi
done

# 3. temp и tmp чистим полностью
for temp_dir in "temp" "tmp"; do
    if [ -d "$temp_dir" ]; then
        echo "Очистка $temp_dir..."
        rm -rf "$temp_dir"/*
        rm -rf "$temp_dir"/.[!.]* 2>/dev/null  # удаляем скрытые файлы
        echo "✓ $temp_dir очищена"
    else
        echo "⚠ Папка $temp_dir не найдена"
    fi
done

# 4. В package удаляем папку lib
if [ -d "package/lib" ]; then
    echo "Удаление package/lib..."
    rm -rf package/lib
    echo "✓ package/lib удалена"
elif [ -d "package" ]; then
    echo "⚠ Папка package/lib не найдена"
else
    echo "⚠ Папка package не найдена"
fi

echo "Очистка проекта завершена!"
