@testset "Multidimensional Arrays" begin
    @testset "#490" begin
        @testset "DefaultStyle" begin
            str = "[1;; 2]"
            @test fmt(str) == str

            str_ = """
            [
                1;;
                2
            ]"""
            @test fmt(str, 4, 1) == str_

            str = "T[1;; 2]"
            @test fmt(str) == str

            str_ = """
            T[
                1;;
                2
            ]"""
            @test fmt(str, 4, 1) == str_
        end

        @testset "YASStyle" begin
            str = "[1;; 2]"
            @test yasfmt(str) == str

            str_ = """
            [1;;
             2]"""
            @test yasfmt(str, 4, 1) == str_

            str = "T[1;; 2]"
            @test yasfmt(str) == str

            str_ = """
            T[1;;
              2]"""
            @test yasfmt(str, 4, 1) == str_
        end
    end

    @testset "#620" begin
        s = "[1; 0;;]"
        @test fmt(s) == s
        @test yasfmt(s) == s
    end

    @testset "#582" begin
        @test yasfmt("[a;b;;]") == "[a; b;;]"
    end

    @testset "#608" begin
        s1 = """
        hcat([zeros(1); ones(3)], [zeros(2); ones(2)], [zeros(3); ones(1)], [zeros(1); ones(3)], [zeros(2); ones(2)], [zeros(3); ones(1)])
        """
        s2 = """
        hcat([zeros(1); ones(3)], [zeros(2); ones(2)], [zeros(3); ones(1)],
             [zeros(1); ones(3)], [zeros(2); ones(2)], [zeros(3); ones(1)])
        """
        @test yasfmt(s1) == s2
    end

    @testset "#532" begin
        s = "(; a = [1;;], b = cos[2;;])"
        @test fmt(s) == s
        @test bluefmt(s) == s
        @test yasfmt(s) == s
        @test format_text(s, SciMLStyle()) == s
        @test minimalfmt(s) == s
    end
end
