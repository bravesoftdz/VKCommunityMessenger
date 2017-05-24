var getNews = function () {
    switch (Math.round(Math.random())) {
        case 0:
            return "На данный момент продукт находится в разработке, которая ведется в нашем GitHub репозитории";
        default:
            return "Two";
    }
}

document.getElementById("newstext").innerHtML = getNews();