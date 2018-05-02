var elem = document.getElementById('draw-animation');
var two = new Two({ width: 285, height: 200 }).appendTo(elem);

var circle = two.makeCircle(-70, 0, 50);
var rect = two.makeRectangle(70, 0, 100, 100);
circle.fill = '#FF8000';
rect.fill = 'rgba(0, 200, 255, 0.75)';

var group = two.makeGroup(circle, rect);
group.translation.set(two.width / 2, two.height / 2);
group.scale = 0;
group.noStroke();

// Bind a function to scale and rotate the group
// to the animation loop.
two.bind('update', function(frameCount) {
  // This code is called everytime two.update() is called.
  // Effectively 60 times per second.
  if (group.scale > 0.9999) {
    group.scale = group.rotation = 0;
  }
  var t = (1 - group.scale) * 0.125;
  group.scale += t;
  group.rotation += t * 4 * Math.PI;
}).play();  // Finally, start the animation loop

// Snake Game Code

Array.prototype.remove = function (val) {
    for (var i = 0; i < this.length; i++) {
        if (this[i] === val) {
            this.splice(i, 1);
            i--;
        }
    }
    return this;
}

var direction = {
    RIGHT : 0,
    UP    : 1,
    LEFT  : 2,
    DOWN  : 3,
}

var turn_direction = {
    CW  : -1,
    CCW :  1,
}

var apples = [];
var snake = {body : [], direction : direction.RIGHT};

function Apple(position) {
    this.position = position;
}

snake.turn = function (turn_direction) {
    snake.direction = (snake.direction + turn_direction) % 4;
}

snake.eatApple = function (apple) {
    snake.body.push(apple.position);
    apples.remove(apple);
}

snake.moveForward = function () {
    snake.body.shift();
    var dx, dy;
    switch (snake.direction) {
        case RIGHT:
            dx =  1; dy =  0;
            break;
        case UP:
            dx =  0; dy =  1;
            break;
        case LEFT:
            dx = -1; dy =  0;
            break;
        case DOWN:
            dx =  0; dy = -1;
            break;
        default:
            break;
    }
    var head = snake.body[0];
    snake.body.push([head[0] + dx, head[1] + dy]);
}

$(document).on("keydown", function (e) {
    document.write(e.which);
});
