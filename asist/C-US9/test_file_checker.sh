#!/bin/bash

folder_path="/root/bin/testbackups"
mysql_folder_path="/root/bin/mysqlbackups"
log_file="/root/checker_logs.log"
db_name="nome_da_db"

# Função para realizar o backup da base de dados
backup_database() {
    local db_name=$1
    local backup_path=$2
    local date_suffix=$(date +%Y%m%d)
    local backup_file="${db_name}_${date_suffix}.sql"

    # Realizar o backup (assumindo MySQL para este exemplo)
    mysqldump -u root -p"${MYSQL_ROOT_PASSWORD}" "$db_name" > "${backup_path}/${backup_file}"
    echo "Backup criado: ${backup_file}" >> "$log_file"
}

# Função para verificar se uma pasta segue os critérios especificados
check_folder() {
    local folder_name=$1
    local test=0

    # Extrair a parte da data
    date_part=${folder_name#test_}

    # Verificar se é o primeiro dia do mês
    if [ "$(date -d "$date_part" +%d)" -eq 28 ]; then
        echo "Pasta '$folder_name' é um backup para o dia 28 do mês. Mantendo-a." >> "$log_file"
        ((test++))
    fi
    # Verificar se é o mês de dezembro e é o primeiro dia da semana
    if [ "$(date -d "$date_part" +%m)" -eq 12 ] && [ "$(date -d "$date_part" +%u)" -eq 1 ]; then
        echo "Pasta '$folder_name' é um backup para o primeiro dia de dezembro. Mantendo-a." >> "$log_file"
        ((test++))
    fi
    # Verificar se é a última semana do ano
    if [ "$(date -d "$date_part" +%W)" -eq 52 ]; then
        echo "Pasta '$folder_name' é um backup para a última semana do ano. Mantendo-a." >> "$log_file"
        ((test++))
    fi
    if [ $test -eq 0 ]; then
        echo "A eliminar a pasta '$folder_name' porque não segue os critérios especificados." >> "$log_file"
        rm -rf "$folder_path/$folder_name"
    fi
}

# Função para verificar se um ficheiro SQL segue os critérios especificados
check_sql_file() {
    local file_name=$1
    local test2=0

    # Extrair a parte da data
    date_part=${file_name#${db_name}_}
    date_part=${date_part%.sql}

    # Verificar se é o primeiro dia do mês
    if [ "$(date -d "$date_part" +%d)" -eq 28 ]; then
        echo "Ficheiro '$file_name' é um backup para o dia 28 do mês. Mantendo-o." >> "$log_file"
        ((test2++))
    fi
    # Verificar se é o mês de dezembro e é o primeiro dia da semana
    if [ "$(date -d "$date_part" +%m)" -eq 12 ] && [ "$(date -d "$date_part" +%u)" -eq 1 ]; then
        echo "Ficheiro '$file_name' é um backup para o primeiro dia de dezembro. Mantendo-o." >> "$log_file"
        ((test2++))
    fi
    # Verificar se é a última semana do ano
    if [ "$(date -d "$date_part" +%W)" -eq 52 ]; then
        echo "Ficheiro '$file_name' é um backup para a última semana do ano. Mantendo-o." >> "$log_file"
        ((test2++))
    fi
    if [ $test2 -eq 0 ]; then
        echo "A eliminar o ficheiro '$file_name' porque não segue os critérios especificados." >> "$log_file"
        rm -rf "$mysql_folder_path/$file_name"
    fi
}

# Realizar o backup da base de dados
backup_database "$db_name" "$mysql_folder_path"

# Verificar se a pasta de backup existe
if [ -d "$folder_path" ]; then
    echo "A verificar pastas em $folder_path..."

    # Percorrer cada subpasta no caminho especificado
    for subfolder in "$folder_path"/*; do
        # Extrair o nome da pasta
        folder_name=$(basename "$subfolder")

        # Verificar a pasta contra os critérios
        check_folder "$folder_name"
    done
else
    echo "Erro: A pasta $folder_path não existe."
fi

# Verificar se a pasta de backup do MySQL existe
if [ -d "$mysql_folder_path" ]; then
    echo "A verificar ficheiros em $mysql_folder_path..."

    # Percorrer cada ficheiro SQL no caminho especificado
    for sql_file in "$mysql_folder_path"/*.sql; do
        # Extrair o nome do ficheiro
        file_name=$(basename "$sql_file")

        # Verificar o ficheiro contra os critérios
        check_sql_file "$file_name"
    done
else
    echo "Erro: A pasta $mysql_folder_path não existe."
fi