#!/bin/bash

declare -A user_counts

while read -r line; do
    if [[ $line == *"Failed password"* ]]; then
        # User
        user=$(echo "$line" | awk '{print $9}')
        # Numero de tentativas falhadas
        ((user_counts["$user"]++))
    fi
done < /var/log/auth.log

echo "Users com mais de 3 tentativas de acesso incorretas:"
for user in "${!user_counts[@]}"; do
    count="${user_counts[$user]}"
    if (( count > 3 )); then
        printf "User: %-8s | Tentativas: %d\n" "$user" "$count"
    fi
done