type 'a pilha = { mutable topo : int; capa : int; pElem : 'a array }

let criarpilha c =
  { topo = -1; capa = c; pElem = Array.make c 0 }

let estavazia p =
  if p.topo = -1 then true
  else false

let estacheia p =
  if p.topo = p.capa - 1 then true
  else false

let empilhar p v =
  p.topo <- p.topo + 1;
  p.pElem.(p.topo) <- v

let desempilhar p =
  let aux = p.pElem.(p.topo) in
  p.topo <- p.topo - 1;
  aux

let retornatopo p =
  p.pElem.(p.topo)

let main () =
  let capacidade = ref 0 in
  let op = ref 0 in
  let valor = ref 0 in
  print_endline "Capacidade da IDs (quantos (Items) valores inteiros vai introduzir no camião)?";
  capacidade := read_int();
  let pilha = criarpilha !capacidade in
  while true do
    print_endline "\n\n1 - Introduzir Item no camião: (Push)";
    print_endline "2 - Retirar Item do camião (Pop)";
    print_endline "3 - Mostrar Item introduzido de ultimo no camião (Peek)";
    print_endline "4 - Mostrar Items no camião";
    print_endline "5 - Sair";
    print_endline "Opcão?";
    op := read_int();
    match !op with
    | 1 ->
      if estacheia pilha then
        print_endline "\n Camião Cheio! \n"
      else begin
        print_endline "\n Item? ";
        valor := read_int();
        empilhar pilha !valor;
      end;
    | 2 ->
      if estavazia pilha then
        print_endline "\n Camião Vazio! \n"
      else begin
        valor := desempilhar pilha;
        print_endline (string_of_int !valor ^ " Item Descarregado! \n");
      end;
    | 3 ->
      if estavazia pilha then
        print_endline "\n Camião Vazio! \n"
      else begin
        valor := retornatopo pilha;
        print_endline (" Ultimo Item Introduzido: " ^ string_of_int !valor ^ "\n\n");
      end;
    | 4 ->
      if estavazia pilha then
        print_endline "\n Camião Vazio! \n"
      else begin
        print_endline "\n Items no Camião: ";
        for i = pilha.topo downto 0 do
          print_endline (string_of_int pilha.pElem.(i));
        done;
      end;
    | 5 -> exit 0;
    | _ -> print_endline "Opção inválida!"
  done;;

main();;
