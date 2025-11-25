# Лабораторная работа №2

## Полиморфное множество на базе AVL-дерева (avl-set)

**Студент:** Сидимеков Дмитрий
**Группа:** P3222

---

В рамках лабораторной работы необходимо было разработать полиморфную неизменяемую структуру данных - множество - на основе AVL-дерева

Операции над структурой данных:

   - добавление элемента;
   - удаление элемента;
   - проверка принадлежности;
   - фильтрация по предикату;
   - отображение (`map`);
   - свёртки: левая и правая (`fold_left`, `fold_right`);
   - множество должно образовывать моноид.

Структура данных должна быть неизменяемой, то есть любая операция возвращает новое множество

---

## Элементы реализации

**Интерфейс множества (функциональный модуль)**

Интерфейс описан в `src/avl_set.mli`. Он включает:

- модуль `OrderedType` - тип с отношением порядка;
- модуль `S` - интерфейс множества;
- функтор `Make` - создание множества для конкретного упорядоченного типа.

Фрагмент:

```ocaml
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t

  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t

  val filter : (elt -> bool) -> t -> t
  val map : (elt -> elt) -> t -> t

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val to_list : t -> elt list
  val of_list : elt list -> t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
```

Эта часть определяет полный API множества.

---

##  Реализация AVL-дерева

Реализация ведётся в файле `src/avl_set.ml`.

### Тип данных

```ocaml
type t =
  | Empty
  | Node of {
      left : t;
      value : elt;
      right : t;
      height : int;
    }
```

AVL-дерево хранит высоту в каждом узле для балансировки.

### Основные функции AVL

- расчёт высоты;
- одно- и двукратные повороты;
- балансировка поддерева;
- вставка (`add`);
- удаление (`remove`);
- поиск минимального (`remove_min`).

Например, балансировка:

```ocaml
let balance t =
  match t with
  | Empty -> Empty
  | Node { left; value; right; _ } as node ->
      let bf = height left - height right in
      if bf > 1 then
        if height (right_of left) > height (left_of left)
        then rotate_right (make (rotate_left left) value right)
        else rotate_right node
      else if bf < -1 then
        if height (left_of right) > height (right_of right)
        then rotate_left (make left value (rotate_right right))
        else rotate_left node
      else make left value right
```

### Функциональные операции

Все операции реализованы через рекурсию и свёртки:

- `filter` через `fold_left`;
- `map` через `fold_left` и `add`;
- `union`, `inter`, `diff` через свёртки и `mem`;
- `fold_left` и `fold_right` через симметричный обход дерева.

### Моноид

Множество образует моноид:

- операция: `union`
- нейтральный элемент: `empty`

Эти свойства проверяются property-тестами.

---

## Тестирование

### Unit-тесты

Unit-тесты описаны в `test/avl_set_test.ml`

Проверяются:

- `empty` и `is_empty`;
- `add`, `mem`;
- отсутствие дубликатов;
- корректность `remove`;
- корректность `of_list`/`to_list`.

Пример:

```ocaml
let test_add_no_duplicates () =
  let s = IntSet.empty |> IntSet.add 1 |> IntSet.add 1 in
  check int "cardinal after dup adds" 1 (IntSet.cardinal s)
```

### QCheck

Генератор множеств:

```ocaml
let gen_int_set =
  QCheck.map IntSet.of_list (QCheck.list QCheck.small_int)
```

Свойства моноида:

```ocaml
union empty s = s
union s empty = s
union is associative
```

Дополнительно:

- `cardinal (of_list l) <= length l`
- `union s s = s`
- `mem x (of_list l) = List.mem x l`

Они автоматически проверяются на случайных данных.

### Результаты тестирования

`dune test`:

```
[OK]        unit: 5 tests passed
[OK]        properties: 5 tests passed
```

## Вывод

В ходе выполнения лабораторной работы была реализована неизменяемая функциональная структура данных - множество avl set - на базе AVL-дерева. Внимание было уделено:

- абстракции через интерфейс `.mli`
- чистым функциям и иммутабельному состоянию
- поддержанию инвариантов AVL-дерева при вставке и удалении
- использованию свёрток и рекурсии
- property-based тестированию, позволяющему проверять математические свойства структуры