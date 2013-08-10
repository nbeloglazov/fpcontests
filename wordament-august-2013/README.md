# wordament

Решение задачи августовского конкурса ФП: http://haskell98.blogspot.co.uk/2013/08/2013.html

Используется Словарь русской литературы: http://speakrus.ru/dict/index.htm

Алгоритм: поиск в глубину с использованием [дерева Бора](http://ru.wikipedia.org/wiki/Префиксное_дерево). Поле не очень большое и поэтому обычный поиск в глубину без особых эвристик работает весьма быстро.

## Запуск

Для запуска нужен [leiningen 2](https://github.com/technomancy/leiningen). Запустить можно при помощи команды:

```shell
lein run input.txt
```

В `input.txt` лежит поле с буквами.

