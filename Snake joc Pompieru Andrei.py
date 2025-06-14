import pygame
import random

pygame.init()
width, height = 1000, 400
win = pygame.display.set_mode((width, height))
pygame.display.set_caption("Snake Game")

white = (255, 255, 255)
black = (0, 0, 0)
red = (255, 50, 80)
green = (0, 255, 0)

snake_block = 10
snake_speed = 15
font_style = pygame.font.SysFont("comicsans", 35)
score_font = pygame.font.SysFont("comicsans", 45)

def score_display(score):
    value = score_font.render("Score: " + str(score), True, red)
    win.blit(value, [0, 0])

def draw_snake(block, snake_list):
    for x in snake_list:
        pygame.draw.rect(win, black, [x[0], x[1], block, block])

def game_loop():
    game_over = False
    game_close = False

    x = width // 2
    y = height // 2
    dx = 0
    dy = 0

    snake_list = []
    snake_len = 1

    food_x = round(random.randrange(0, width - snake_block) / 10.0) * 10.0
    food_y = round(random.randrange(0, height - snake_block) / 10.0) * 10.0

    clock = pygame.time.Clock()

    while not game_over:

        while game_close:
            win.fill(white)
            msg = font_style.render("Game Over! Press C to Continue or Q to Quit", True, red)
            win.blit(msg, [width / 6, height / 3])
            score_display(snake_len - 1)
            pygame.display.update()

            for event in pygame.event.get():
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_q:
                        game_over = True
                        game_close = False
                    if event.key == pygame.K_c:
                        game_loop()

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                game_over = True
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    dx = -snake_block
                    dy = 0
                elif event.key == pygame.K_RIGHT:
                    dx = snake_block
                    dy = 0
                elif event.key == pygame.K_UP:
                    dy = -snake_block
                    dx = 0
                elif event.key == pygame.K_DOWN:
                    dy = snake_block
                    dx = 0

        if x >= width or x < 0 or y >= height or y < 0:
            game_close = True

        x += dx
        y += dy
        win.fill(white)
        pygame.draw.rect(win, green, [food_x, food_y, snake_block, snake_block])

        snake_head = [x, y]
        snake_list.append(snake_head)

        if len(snake_list) > snake_len:
            del snake_list[0]

        for block in snake_list[:-1]:
            if block == snake_head:
                game_close = True

        draw_snake(snake_block, snake_list)
        score_display(snake_len - 1)

        pygame.display.update()

        if x == food_x and y == food_y:
            food_x = round(random.randrange(0, width - snake_block) / 10.0) * 10.0
            food_y = round(random.randrange(0, height - snake_block) / 10.0) * 10.0
            snake_len += 1

        clock.tick(snake_speed)

    pygame.quit()
    quit()


game_loop()
