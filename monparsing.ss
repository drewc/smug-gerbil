(module MonadClasses
  (import :drewc/smug/interface)
  (export #t Monad::interface)
  (define-interface-class Monad
    (result bind))
  (define-interface-class (Monad0Plus Monad)
    (zero ++)))

(import :std/iter :std/srfi/13 (for-syntax :drewc/smug/interface)
        :drewc/smug/interface
        (for-syntax MonadClasses)
        MonadClasses)


