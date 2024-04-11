-module(erws_mnesia).
-export([setup_tables/0, create_auction_table/0,save_auction_pid/1,print_mnesia_content/0]).

-record(auction, {pid, start_date, end_date}).

setup_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_auction_table().

create_auction_table() ->
    case mnesia:create_table(auction, [{attributes, record_info(fields, auction)}]) of
        {atomic, ok} ->
            ok;
        {atomic, {already_exists, _Table}} ->
            ok; % Tabella già esistente, restituisci semplicemente ok
        {aborted, Reason} ->
            io:format("Error creating auction table: ~p~n", [Reason])
    end.


save_auction_pid(AuctionPid) ->
    % Avvia una transazione
    mnesia:transaction(fun() ->
        Record = #auction{pid = AuctionPid},
        % Esegui l'operazione di scrittura all'interno della transazione
        case mnesia:write(Record) of
            {atomic, ok} ->
                ok;
            {aborted, Reason} ->
                % Se si verifica un errore durante la scrittura, stampa il motivo dell'errore
                io:format("Error saving auction PID: ~p~n", [Reason])
        end
    end).


% Funzione per stampare il contenuto del database Mnesia
print_mnesia_content() ->
    % Ottenere tutte le chiavi dei record
    Keys = mnesia:dirty_all_keys(auction),
    % Recupera e stampa ogni record
    lists:foreach(
        fun(Key) ->
            case mnesia:dirty_read({auction, Key}) of
                {atomic, Record} ->
                    io:format("Record: ~p~n", [Record]);
                _ ->
                    io:format("Failed to read record with key ~p~n", [Key])
            end
        end,
        Keys
    ).

