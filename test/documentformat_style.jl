dffmt1(s, i, m; kwargs...) = fmt1(s; kwargs..., i = i, m = m, style = DocumentFormatSyle())
dffmt(s, i, m; kwargs...) = fmt(s; kwargs..., i = i, m = m, style = DocumentFormatStyle())

@testset "DocumentFormat style" begin
    @testset "basic" begin
        str = "a = (arg1, arg2, arg3)"
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = (arg1, arg2,
             arg3)"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = (arg1,
             arg2,
             arg3)"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = "a = [arg1, arg2, arg3]"
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = [arg1, arg2,
             arg3]"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = [arg1,
             arg2,
             arg3]"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str_ = "a = {arg1, arg2, arg3}"
        str = """
        a = {arg1,arg2,arg3}"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = {arg1,arg2,
             arg3}"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = {arg1,
             arg2,
             arg3}"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str_ = "a = Union{ arg1, arg2, arg3 }"
        str = """
        a = Union{arg1,arg2,arg3}"""
        @test dffmt(str_, 4, 1) == str
        @test dffmt(str_, 4, 100) == str
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = Union{arg1,arg2,
                  arg3}"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = Union{arg1,
                  arg2,
                  arg3}"""
        @test dffmt(str, 4, 100) == str
        @test dffmt(str, 4, 100) == str

        str_ = "a = fcall( arg1,arg2,arg3 )"
        str = """
        a = fcall(arg1, arg2, arg3)"""
        @test dffmt(str_, 4, 1) == str
        @test dffmt(str_, 4, 100) == str
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = fcall(arg1, arg2,
                  arg3)"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = fcall(arg1,
                  arg2,
                  arg3)"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str_ = "a = @call(  arg1,arg2,arg3 )"
        str = """
        a = @call(arg1, arg2, arg3)"""
        @test dffmt(str_, 4, 1) == str
        @test dffmt(str_, 4, 100) == str
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = @call(arg1, arg2,
                  arg3)"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = @call(arg1,
                  arg2,
                  arg3)"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str_ = "a = array[ arg1,arg2,arg3  ]"
        str = """
        a = array[arg1, arg2, arg3]"""
        @test dffmt(str_, 4, 1) == str
        @test dffmt(str_, 4, 100) == str
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = array[arg1, arg2,
                  arg3]"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        a = array[arg1,
                  arg2,
                  arg3]"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        using Cassette: A, B, C"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        using Cassette: A, B,
                        C"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        using Cassette: A,
                        B, C"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str

        str = """
        using Cassette: A,
                        B,
                        C"""
        @test dffmt(str, 4, 1) == str
        @test dffmt(str, 4, 100) == str
    end

    # more complicated samples
    @testset "pretty" begin
        str_ = "comp = [a * b for a in 1:10, b in 11:20]"

        str = """
        comp = [a * b
                for a in 1:10, b in 11:20]"""
        @test dffmt(str_, 2, length(str_) - 1, always_for_in = true) == str
        @test dffmt(str_, 2, 34, always_for_in = true) == str

        str = """
        comp = [a * b
                for a in 1:10,
                    b in 11:20]"""
        @test dffmt(str_, 2, 33, always_for_in = true) == str

        str = """
        comp = [a *
                b
                for a in
                    1:10,
                    b in
                    11:20]"""
        @test dffmt(str_, 2, 1, always_for_in = true) == str

        str_ = "comp = Typed[a * b for a in 1:10, b in 11:20]"
        str = """
        comp = Typed[a * b
                     for a in 1:10, b in 11:20]"""
        @test dffmt(str_, 2, length(str_) - 1, always_for_in = true) == str
        @test dffmt(str_, 2, 39, always_for_in = true) == str

        str = """
        comp = Typed[a * b
                     for a in 1:10,
                         b in 11:20]"""
        @test dffmt(str_, 2, 38, always_for_in = true) == str

        str = """
        comp = Typed[a *
                     b
                     for a in
                         1:10,
                         b in
                         11:20]"""
        @test dffmt(str_, 2, 1, always_for_in = true) == str

        str_ = "foo(arg1, arg2, arg3) == bar(arg1, arg2, arg3)"
        str = """
        foo(arg1, arg2, arg3) ==
        bar(arg1, arg2, arg3)"""
        @test dffmt(str, 2, length(str_)) == str_
        @test dffmt(str_, 2, length(str_) - 1) == str
        @test dffmt(str_, 2, 24) == str

        str = """
        foo(arg1, arg2,
            arg3) ==
        bar(arg1, arg2, arg3)"""
        @test dffmt(str_, 2, 23) == str
        @test dffmt(str_, 2, 21) == str

        str = """
        foo(arg1, arg2,
            arg3) ==
        bar(arg1, arg2,
            arg3)"""
        @test dffmt(str_, 2, 20) == str
        @test dffmt(str_, 2, 15) == str

        str = """
        foo(arg1,
            arg2,
            arg3) ==
        bar(arg1,
            arg2,
            arg3)"""
        @test dffmt(str_, 2, 14) == str
        @test dffmt(str_, 2, 1) == str

        str_ = """
        function func(arg1::Type1, arg2::Type2, arg3) where {Type1,Type2}
          body
        end"""
        str = """
        function func(arg1::Type1, arg2::Type2,
                      arg3) where {Type1,Type2}
          body
        end"""
        @test dffmt(str_, 2, 64) == str
        @test dffmt(str_, 2, 39) == str

        str = """
        function func(arg1::Type1,
                      arg2::Type2,
                      arg3) where {Type1,
                                   Type2}
          body
        end"""
        @test dffmt(str_, 2, 31) == str
        @test dffmt(str_, 2, 1) == str

        str_ = """
        @test TimeSpan(spike_annotation) == TimeSpan(first(spike_annotation), last(spike_annotation))"""
        str = """
        @test TimeSpan(spike_annotation) ==
              TimeSpan(first(spike_annotation), last(spike_annotation))"""
        @test dffmt(str_, 4, length(str_) - 1) == str
        @test dffmt(str_, 4, 63) == str
        str_ = """
        @test TimeSpan(spike_annotation) == TimeSpan(first(spike_annotation), last(spike_annotation))"""
        str = """
        @test TimeSpan(spike_annotation) ==
              TimeSpan(first(spike_annotation),
                       last(spike_annotation))"""
        @test dffmt(str_, 4, 62) == str

        str_ = raw"""ecg_signal = signal_from_template(eeg_signal; channel_names=[:avl, :avr], file_extension=Symbol("lpcm.zst"))"""
        str = raw"""
        ecg_signal = signal_from_template(eeg_signal; channel_names = [:avl, :avr],
                                          file_extension = Symbol("lpcm.zst"))"""
        @test dffmt(str_, 4, length(str_) - 1) == str
        @test dffmt(str_, 4, 75) == str

        str = raw"""
        ecg_signal = signal_from_template(eeg_signal;
                                          channel_names = [:avl, :avr],
                                          file_extension = Symbol("lpcm.zst"))"""
        @test dffmt(str_, 4, 73) == str
        str = raw"""
        ecg_signal = signal_from_template(eeg_signal;
                                          channel_names = [:avl,
                                                           :avr],
                                          file_extension = Symbol("lpcm.zst"))"""
        @test dffmt(str_, 4, 1) == str

    end

    @testset "inline comments with arguments" begin
        str_ = """
        var = fcall(arg1,
            arg2, arg3, # comment
                            arg4, arg5)"""
        str = """
        var = fcall(arg1, arg2, arg3, # comment
                    arg4, arg5)"""
        @test dffmt(str_, 4, 80) == str
        @test dffmt(str_, 4, 29) == str

        str = """
        var = fcall(arg1, arg2,
                    arg3, # comment
                    arg4, arg5)"""
        @test dffmt(str_, 4, 28) == str
        @test dffmt(str_, 4, 23) == str

        str = """
        var = fcall(arg1,
                    arg2,
                    arg3, # comment
                    arg4,
                    arg5)"""
        @test dffmt(str_, 4, 22) == str
        @test dffmt(str_, 4, 1) == str

        str_ = """
        comp = [
        begin
                    x = a * b + c
                    y = x^2 + 3x # comment 1
            end
                       for a in 1:10,  # comment 2
                    b in 11:20,
           c in 300:400]"""

        str = """
        comp = [begin
                  x = a * b + c
                  y = x^2 + 3x # comment 1
                end
                for a = 1:10,  # comment 2
                    b = 11:20, c = 300:400]"""
        @test dffmt(str_, 2, 80) == str
        @test dffmt(str_, 2, 35) == str

        str = """
        comp = [begin
                  x = a * b + c
                  y = x^2 + 3x # comment 1
                end
                for a = 1:10,  # comment 2
                    b = 11:20,
                    c = 300:400]"""
        @test dffmt(str_, 2, 34) == str

        str_ = """
        ys = ( if p1(x)
                 f1(x)
        elseif p2(x)
            f2(x)
        else
            f3(x)
        end for    x in xs)
        """
        str = """
        ys = (if p1(x)
                f1(x)
              elseif p2(x)
                f2(x)
              else
                f3(x)
              end
              for x in xs)
        """
        @test dffmt(str_, 2, 80) == str

        str_ = """spike_annotation = first(ann for ann in recording.annotations if ann.value == "epileptiform_spike")"""
        str = """
        spike_annotation = first(ann
                                 for ann in recording.annotations
                                 if ann.value == "epileptiform_spike")"""
        @test dffmt(str_, 2, 80) == str

        # only that
        str_ = "foo(a, b) = (arg1, arg2, arg3)"
        str = """
        foo(a, b) = (arg1, arg2,
                     arg3)"""
        @test dffmt(str_, 2, length(str_) - 1) == str

        str = """
        foo(a, b) = (arg1,
                     arg2,
                     arg3)"""
        @test dffmt(str_, 2, 1) == str

        str_ = """
        fooooooooooooooooooo(arg1, arg2, 
        x -> begin
        body
        end
        )"""
        str = """
        fooooooooooooooooooo(arg1, arg2,
                             x -> begin
                                 body
                             end)"""
        @test dffmt(str_, 4, 32) == str

    end

    @testset "inline comments with arguments" begin
        # parsing error when newline is placed front of `for` here
        str_ = "var = (x, y) for x = 1:10, y = 1:10"
        str = """
        var = (x, y) for x = 1:10,
                         y = 1:10"""
        @test dffmt(str_, 4, length(str_) - 1) == str
    end

    @testset "invisbrackets" begin
        str_ = """
        if ((
          aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ||
          aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ||
          aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        ))
          nothing
        end"""
        str = """
        if ((aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ||
             aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ||
             aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa))
          nothing
        end"""
        @test dffmt(str_, 2, 92) == str
    end

    @testset "issue 189" begin
        str_ = """
    D2 = [
            (b_hat * y - delta_hat[i] * y) * gamma[i] + (b * y_hat - delta[i] * y_hat) *
                                                            gamma_hat[i] + (b_hat - y_hat) *
                                                                           delta[i] + (b - y) *
                                                                                      delta_hat[i] - delta[i] * delta_hat[i]
            for i = 1:8
        ]"""
        str = """
        D2 = [(b_hat * y - delta_hat[i] * y) * gamma[i] +
              (b * y_hat - delta[i] * y_hat) * gamma_hat[i] +
              (b_hat - y_hat) * delta[i] +
              (b - y) * delta_hat[i] - delta[i] * delta_hat[i]
              for i = 1:8]"""
        @test dffmt(str_, 2, 60) == str

    end

    @testset "issue 237" begin
        str_ = """
        for x in (arg1, arg2,)
            @info "Test"
        end"""
        str = """
        for x in (arg1, arg2)
            @info "Test"
        end"""
        @test dffmt(str_, 4, 92) == str
    end

end
