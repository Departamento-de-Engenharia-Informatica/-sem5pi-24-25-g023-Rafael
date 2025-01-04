#!/bin/bash

DB_HOST="vsgate-s1.dei.isep.ipp.pt"
DB_PORT="1234"
DB_NAME="sem5pi"
DB_USER="root"
DB_PASSWORD="trabalhasemuito2425"

BACKUP_DIR="/backup/database"
BACKUP_OLD="$BACKUP_DIR/backups"
LAST_BACKUP_TIME_FILE="$BACKUP_DIR/last_backup_time.txt"

DATE=$(date "+%Y-%m-%d_%H-%M-%S")

# Criar diretorios de backup, caso nao existam
mkdir -p "$BACKUP_DIR"
mkdir -p "$BACKUP_OLD"

# Verificar ultimo backup
if [ ! -f "$LAST_BACKUP_TIME_FILE" ]; then
  echo "1970-01-01 00:00:00" > "$LAST_BACKUP_TIME_FILE"
else
    LAST_BACKUP_TIME=$(cat "$LAST_BACKUP_TIME_FILE")
fi


# Backup total
if [ "$(date '+%u')" -eq 7 ]; then
  echo "A criar Backup Completo..."
  
  # Mover backups antigos
  mv "$BACKUP_DIR"/*.tar.gz "$BACKUP_OLD"

  # Realizar backup total
  mysqldump -h "$DB_HOST" --port="$DB_PORT" -u "$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" > "$BACKUP_DIR/full_backup_$DATE.sql"
  
  # Compactar o backup
  tar -czf "$BACKUP_DIR/full_backup_$DATE.tar.gz" -C "$BACKUP_DIR" "full_backup_$DATE.sql"
  rm "$BACKUP_DIR/full_backup_$DATE.sql"

  # Atualizar o horario
  echo "$(date '+%Y-%m-%d %H:%M:%S')" > "$LAST_BACKUP_TIME_FILE"
  
  echo "Backup total concluido: $BACKUP_DIR/full_backup_$DATE.tar.gz"
else
  # Backup incremental
  echo "A criar Backup Incremental desde $LAST_BACKUP_TIME..."
  
  # Listar tabelas
  TABLES=$(mysql -h "$DB_HOST" -P "$DB_PORT" -u "$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" -e "SHOW TABLES;" | tail -n +2)
  
  # Backup incremental para cada tabela modificada
  for TABLE in $TABLES; do
    QUERY="SELECT * FROM $TABLE WHERE updated_at >= '$LAST_BACKUP_TIME' OR created_at >= '$LAST_BACKUP_TIME';"
    mysql -h "$DB_HOST" -P "$DB_PORT" -u "$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" -e "$QUERY" > "$BACKUP_DIR/incremental_$TABLE_$DATE.sql"
  done

  # Compactar os backups
  tar -czf "$BACKUP_DIR/incremental_backup_$DATE.tar.gz" -C "$BACKUP_DIR" "incremental_"*.sql
  rm "$BACKUP_DIR/incremental_"*.sql

  # Atualizar o horario
  echo "$(date '+%Y-%m-%d %H:%M:%S')" > "$LAST_BACKUP_TIME_FILE"
  
  echo "Backup incremental concluido: $BACKUP_DIR/incremental_backup_$DATE.tar.gz"
fi

echo "Backup concluido com sucesso"