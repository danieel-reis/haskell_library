--Alunos: Daniel Martins Reis / Brenda Leite e Lima
--Matricula: 14.1.8295 / 11.2.8147
--Descrição: TP2 Biblioteca

module Main (main) where

import System.IO
import Control.Exception
import System.IO.Error




--------------------------------------------------------------- DECLARACAO DE VARIAVEIS ---------------------------------------------------------------
--Definicao do nome do programa
nomePrograma::String
nomePrograma = "Biblioteca Haskell"

--Definicao do tamanho da linha para a impressao
tamanhoLinha::Int
tamanhoLinha = 70

--LivroPath
livroPath::FilePath
livroPath = "livros.txt"

--CupomPath
cupomPath::FilePath
cupomPath = "cupom.txt"

--MarcadorFim
marcadorFim::String
marcadorFim = "-----------------------------------//-----------------------------------"




----------------------------------------------------------------- TIPO DAS VARIAVEIS ------------------------------------------------------------------
--Tipos das variaveis
type Codigo = Int
type Nome = String
type Autor = String
type Volume = Int
type Ano = Int
type Editora = String
type Preco = Int

type Livro = (Codigo, Nome, Autor, Volume, Ano, Editora, Preco)
type Item = (Nome,Preco)




------------------------------------------------------------------------- GET -------------------------------------------------------------------------
--Get
getCodigo::Livro->Codigo
getCodigo (codigo, _, _, _, _, _, _) = codigo

getNome::Livro->Nome
getNome (_, nome, _, _, _, _, _) = nome

getAutor::Livro->Autor
getAutor (_, _, autor, _, _, _, _) = autor

getVolume::Livro->Volume
getVolume (_, _, _, volume, _, _, _) = volume

getAno::Livro->Ano
getAno (_, _, _, _, ano, _, _) = ano

getEditora::Livro->Editora
getEditora (_, _, _, _, _, editora, _) = editora

getPreco::Livro->Preco
getPreco (_, _, _, _, _, _, preco) = preco




---------------------------------------------------------------------- IMPRESSAO ----------------------------------------------------------------------
--Formata os dados de um item
formataItem::Item->String
formataItem (n, p) = "\t" ++ n ++ "\tPreco: " ++ formataMoeda p

--Formata os dados de um livro
formataLivro::Livro->String
formataLivro (co, no, au, vo, an, ed, pr) = texto
    where
        qtdespacosn = tamanhoLinha - length(no)
        espacosn = replicate qtdespacosn ' '
        qtdespacosa = 25 - length(au)
        espacosa = replicate qtdespacosa ' '
        qtdespacose = 10 - length(ed)
        espacose = replicate qtdespacose ' '
        texto = "\t" ++ show co ++ "\t" ++ no ++ espacosn ++ "\t" ++ au ++ espacosa ++ "\t" ++ show vo ++ "\t" ++ show an ++ "\t" ++ ed ++ espacose ++ "\t" ++ formataMoeda pr ++ "\n"

--Formata lista de livros
formataLivros::[Livro]->String
formataLivros [] = []
formataLivros (x:xs) = formataLivro x ++ formataLivros xs

--Exibe todos livros
exibeLivros::[Livro]->String
exibeLivros [] = "Nenhum livro cadastrado\n"
exibeLivros x = cabecalho ++ formataLivros x
    where
        co = "Codigo"
        no = "Nome"
        au = "Autor"
        vo = "Volume"
        an = "Ano"
        ed = "Editora"
        pr = "Preco"
        qtdespacosn = tamanhoLinha - length(no)
        espacosn = replicate qtdespacosn ' '
        qtdespacosa = 25 - length(au)
        espacosa = replicate qtdespacosa ' '
        qtdespacose = 10 - length(ed)
        espacose = replicate qtdespacose ' '
        cabecalho = "\t" ++ co ++ "\t" ++ no ++ espacosn ++ "\t" ++ au ++ espacosa ++ "\t" ++ vo ++ "\t" ++ an ++ "\t" ++ ed ++ espacose ++ "\t" ++ pr ++ "\n"




----------------------------------------------------------------- ADICIONA NO ARQUIVO -----------------------------------------------------------------
--Adiciona Livro
adicionaLivro::IO()
adicionaLivro = do
        putStrLn "Informe o codigo do livro (Ex: 1): "
        co <- getLine
        putStrLn "Informe o nome do livro (Ex: O_Codigo_da_Vinci): "
        no <- getLine
        putStrLn "Informe o autor do livro (Ex: Dan_Brown): "
        au <- getLine
        putStrLn "Informe o volume do livro (Ex: 1):"
        vo <- getLine
        putStrLn "Informe o ano do livro (Ex: 2017): "
        an <- getLine
        putStrLn "Informe a editora do livro (Ex: Saraiva): "
        ed <- getLine
        putStrLn "Informe o preco do livro (em centavos) -  (Ex: 10190): "
        pr <- getLine
        appendFile livroPath (co ++ "\t" ++ no ++ "\t" ++ au ++ "\t" ++ vo ++ "\t" ++ an ++ "\t" ++ ed ++ "\t" ++ pr ++ "\n")
        putStrLn(">>> Livro salvo <<<")
        putStrLn("Deseja inserir mais livros? s/n")
        resposta <- getLine
        if ((resposta == "s")) then adicionaLivro else return()

--Adiciona Cupom
adicionaCupom::[Livro]->[Codigo]->IO()
adicionaCupom livros codigos = salvaCupom (formataAluguel (montaMochila livros codigos))

salvaCupom::String->IO()
salvaCupom x = do
    writeFile cupomPath (x ++ "\n" ++ marcadorFim ++ "\n\n")
    putStrLn(">>> Cupom salvo <<<")




------------------------------------------------------------------ LEITURA DO ARQUIVO -----------------------------------------------------------------
--Gera lista de livros
geraListaLivros::[[String]]->[Livro]
geraListaLivros [] = []
geraListaLivros ([co, no, au, vo, an, ed, pr]:[]) = [(read co::Int, no, au, read vo::Int, read an::Int, ed, read pr::Int)]
geraListaLivros ([co, no, au, vo, an, ed, pr]:xs) = (read co::Int, no, au, read vo::Int, read an::Int, ed, read pr::Int): geraListaLivros xs

--Captura do arquivo a lista de livros
listaLivros::IO [Livro]
listaLivros = do
    s <- readFile livroPath
    return (geraListaLivros(map words(lines s)))

--Ler ultimo cupom
lerUltimoCupom::IO()
lerUltimoCupom = do
  {catch (leitura) tratar_erro}
  where
    leitura = do
    {
        conteudo <- readFile cupomPath;
        putStrLn conteudo;
        return (read conteudo);
    }
    tratar_erro erro = if isDoesNotExistError erro
    then do
    {
        putStrLn "Excecao: arquivo inexistente";
        --tratando excecao.
    }
    else ioError erro;



------------------------------------------------------------------- FUNCIONALIDADES -------------------------------------------------------------------
--Recebe um preco em centavos e retorna o valor em reais
formataMoeda::Preco->String
formataMoeda n
    | centavos < 10 = show(reais) ++ ",0" ++ show(centavos)
    | otherwise = show(reais) ++ "," ++ show(centavos)
    where
        reais = div n 100
        centavos = mod n 100

--Formata os dados de uma linha, mostrando o nome.........preco
formataLinha::Item->String
formataLinha (nome, preco) = nome ++ pontos ++ valor ++ "\n"
    where
        lnome = length(nome)
        valor = formataMoeda(preco)
        lmoeda = length(valor)
        qtdpontos = tamanhoLinha - lnome - lmoeda
        pontos = replicate qtdpontos '.'

--Formata o comprovante. Nesse caso, e preciso formatar cada elemento da lista, que se refere a uma linha
formataComprovante::[Item]->String
formataComprovante [] = []
formataComprovante (x:xs) = "  " ++ formataLinha(x) ++ formataComprovante(xs)

--Formata o valor total, mostrando o total.........preco
formataTotal::Preco->String
formataTotal preco = "  " ++ formataLinha("Total:", preco)

--Formata aluguel: junta as funcoes necessarias pra criar o comprovante. Sum recebe a soma de todos os itens
somaItens::[Item]->Preco
somaItens [] = 0
somaItens ((_, preco):xs) = preco + somaItens(xs)

formataAluguel::[Item]->String
formataAluguel x = espacos ++ nomePrograma ++ "\n\n" ++ formataComprovante x ++ "\n\n" ++ formataTotal total
    where
        lnome = length(nomePrograma)
        qtdespacos = div (tamanhoLinha - lnome) 2
        espacos = replicate qtdespacos ' '
        total = somaItens x

--PesquisaLivro: verifica se o livro esta cadastrado retornando o nome ou Não encontrado",0.
pesquisaLivro::[Livro]->Codigo->Item
pesquisaLivro [] _ = ("Nao encontrado!", 0)
pesquisaLivro (x:xs) codigo
    | getCodigo x == codigo = (getNome x, getPreco x)
    | otherwise = pesquisaLivro xs codigo

--BuscaLivro: testa se um livro esta presente ou nao
buscaLivro::[Livro]->Codigo->String
buscaLivro [] _ = "Nao encontrado!"
buscaLivro (x:xs) codigo
    | getCodigo x == codigo = "Encontrado!"
    | otherwise = buscaLivro xs codigo

---montaMochila: recebe uma lista de livros e rotorna uma lista de alugueis
montaMochila::[Livro]->[Codigo]->[Item]
montaMochila livros codigos = map (pesquisaLivro livros) codigos

--fazEmprestimo: recebe uma lista de codigos de livros e retorna uma string
fazEmprestimo::[Livro]->[Codigo]->String
fazEmprestimo livros codigos = formataAluguel (montaMochila livros codigos)




------------------------------------------------------------------------ MENU -------------------------------------------------------------------------
--Menu
main::IO()
main = do
    putStrLn ("------Opcoes------\n 1- Exibir Livros\n 2- Adicionar Livro\n 3- Realizar Emprestimo\n 4- Pesquisar Livros\n 5- Visualizar ultimo aluguel\n 6- Sair")
    op <- readLn
    case op of
        1 -> exibe
        2 -> addLivro
        3 -> emprestimo
        4 -> busca
        5 -> visualizaUltimoAluguel
        6 -> return()
        otherwise -> main

exibe::IO()
exibe = do
    livros <- listaLivros
    putStrLn (exibeLivros livros)
    main

addLivro::IO()
addLivro = do
    putStrLn ""
    adicionaLivro
    putStrLn ""
    main

emprestimo::IO()
emprestimo = do
    livros <- listaLivros
    putStr "\nEmprestimo\n Informe o codigo dos livros (Ex: [1,2,3...]):\n"
    codigos <- getLine 
    putStrLn (fazEmprestimo livros (read codigos::[Codigo]))
    adicionaCupom livros (read codigos::[Codigo])
    main

busca::IO()
busca = do
    livros <- listaLivros
    putStr "\nPesquisa de Livro\n Informe o codigo do livro (Ex: 1):\n"
    codigo <- getLine
    if((buscaLivro livros (read codigo::Codigo)) == "Encontrado!")
    then do
        putStrLn "Livro Encontrado"
        putStrLn (formataItem (pesquisaLivro livros (read codigo::Codigo)))
        putStr "Deseja alugar livro pesquisado ?\n"
        emprestimo <- getLine
        case emprestimo of
            "s" -> do
                putStrLn ("\n" ++ (fazEmprestimo livros ((read codigo::Codigo):[])))
                adicionaCupom livros ((read codigo::Codigo):[])
            "n" -> putStrLn ""
            otherwise -> putStrLn "\nOpcao invalida\n"
        main
    else do
        putStrLn "Livro nao encontrado\n"
        main

visualizaUltimoAluguel::IO()
visualizaUltimoAluguel = do
    lerUltimoCupom
    main

--Testado online em: https://www.tutorialspoint.com/compile_haskell_online.php
