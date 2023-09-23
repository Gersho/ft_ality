up:
	docker run -d --name ft_ality ft_ality_image

build:
	docker build -t ft_ality_image .

exec:
	docker exec -it ft_ality bash

down: 
	docker stop ft_ality || true

rm:
	docker rm ft_ality || true

fclean: down
	docker system prune -af

re: down rm build up exec

exec_test:
	docker exec ft_ality bash -c "echo a ; echo b ; echo c; exit;"