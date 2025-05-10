#include <stdio.h>
#include <string.h>
#include "raylib.h"

int main(int argc, char *argv[])
{
    // Переменные для хранения кодов шейдеров
    const char *vertexShaderCode = NULL;
    const char *fragmentShaderCode = NULL;
    
    // Парсинг аргументов командной строки
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-vs") == 0 && i+1 < argc) {
            vertexShaderCode = argv[i+1];
            i++;  // Пропускаем следующий аргумент (значение шейдера)
        }
        else if (strcmp(argv[i], "-fs") == 0 && i+1 < argc) {
            fragmentShaderCode = argv[i+1];
            i++;  // Пропускаем следующий аргумент (значение шейдера)
        }
    }

    // Проверка, что хотя бы один шейдер передан
    if (vertexShaderCode == NULL && fragmentShaderCode == NULL) {
        printf("Usage: %s -vs \"vertex_code\" -fs \"fragment_code\"\n", argv[0]);
        printf("Note: At least one shader (-vs or -fs) must be provided.\n");
        return 1;
    }

    // Инициализация окна
    InitWindow(0, 0, "RayLib Shader Compiler");
    SetTargetFPS(60);

    // Загрузка шейдера
    Shader shader = LoadShaderFromMemory(vertexShaderCode, fragmentShaderCode);

    // Проверка успешности загрузки
    if (shader.id == 0) {
        printf("Error: Failed to load shader! Check your GLSL code.\n");
    } else {
        printf("Shader loaded successfully!\n");
    }

    // Очистка
    UnloadShader(shader);
    CloseWindow();

    return 0;
}
