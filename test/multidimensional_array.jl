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

            str = "[v1 v2;;]"
            @test yasfmt(str) == str
            @test yasfmt(str, 4, 1) == str

            str = "T[v1 v2;;]"
            @test yasfmt(str) == str
            @test yasfmt(str, 4, 1) == str
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
end
