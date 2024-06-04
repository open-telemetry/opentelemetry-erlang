-define(current_ctx, otel_ctx:get_current()).

-define(with_ctx(Ctx, Fun),
        otel_ctx:with_ctx(Ctx, Fun)).
