# HW4-A checker (Q1, Q2)

K- 인터프리터 과제 1, 2번 체커입니다. 테스트케이스는 hw3처럼 파일별로
`examples/`(4-1), `exercises/`(4-2)에 들어 있고, `check`가 학생 인터프리터로
실행해 `.ans`와 비교합니다. 케이스는 `PL_sample_answers_2026.txt`의 `4-1`, `4-2`
섹션에서 가져왔습니다.

## How to use

### 4-1 (K- 인터프리터)
1. 구현한 `k.ml`을 `lib/k.ml`로 복사합니다.
2. `./check 1` (또는 `./check`).

### 4-2 (트리 라이브러리, K- 코드)
1. 제출 코드를 `tree.k-`로 복사합니다. 제출 스펙대로 소스 마지막을
   ```
   ...
   in
   2026
   ```
   로 끝내야 합니다. 채점기가 마지막 `2026` 자리에 각 테스트 코드를 주입합니다.
2. `./check 2` (또는 `./check`).

- `./check` : 1, 2번 모두 검사
- `./check 1` / `./check 2` : 해당 번호만 검사

## 구조
- `lib/` : 조교 제공 뼈대(`lexer.mll`, `parser.mly`, `pp.ml`) + 학생 `k.ml`. dune
  라이브러리 `k_`.
- `bin/` : 인터프리터 실행파일(`main.ml`). 빌드하면 `_build/default/bin/main.exe`.
- `examples/testNN.k-` + `testNN.ans` : 4-1 테스트(필요하면 `testNN.in`도).
- `exercises/testNN.k-` + `testNN.ans` : 4-2 테스트. `.k-`는 `tree.k-`의 `2026`
  자리에 주입되는 코드 조각입니다.
- `tree.k-` : 4-2 제출(트리 라이브러리). 마지막이 `2026`이어야 합니다.

## 테스트케이스
파일만 추가하면 됩니다(생성기 불필요). `examples/`에 `testNN.k-`와 `testNN.ans`를
넣으면 `./check`가 자동으로 잡습니다.

`.ans` 비교 규칙:
- 보통은 stdout과 정확히 일치해야 합니다.
- `.ans`가 `Runtime Error :`로 시작하면 출력이 그 문자열로 **시작**하는지만 봅니다
  (에러 종류 비교).
- 인터프리터는 `write`만 출력하고 프로그램의 반환값은 출력하지 않으므로, 값 자체를
  검사하려면 `examples/testNN.k-`에서 `write ( ... )`로 감싸 값을 출력시킵니다
  (자동 생성된 값 케이스들이 이렇게 되어 있습니다).
- 각 테스트는 10초 timeout이 걸려 있어, 무한 루프 풀이도 해당 케이스만 실패 처리됩니다.
