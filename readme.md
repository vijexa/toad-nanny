# Toad Nanny

Или **жабоняня**

## What is it?

**It's a simple bot for a russian tamagochi-like game with toads. The rest of readme will be in russian.**

## Что это?

Жабоняня - бот для кормления и посылания на работу/забирания с работы ваших жабок. Что за жабки? https://vk.com/toadbot

## Чего ожидать?

При запуске бот отправит "жаба инфо" в беседу. Исходя из полученных данных бот либо совершит действие (покормит/отправит на работу/заберет с работы), либо отправится в сон до ближайшей возможности совершить действие. Через 5 секунд после совершения действия бот опять отправит "жаба инфо" чтобы узнать, сколько ждать до ближайшей возможности совершить действие. Таким образом бот работает как на обычных, так и на премиум жабах (с укороченными кулдаунами) без дополнительной конфигурации.

## Как запустить

Во вкладке releases репозитория найти последний .jar файл. Нужна Java 11 (должно работать и с 8, не проверял, но скомпилено под 11)
Запускается вот так:

```shell
java -jar toad-nanny-assembly-*version*.jar --token *ваш API токен* --groupID *айди вашей беседы*
```

Токен должен иметь право доступа messages, а также желательно быть бессрочным (offline). К сожалению, ВК запретил выдавать токены с правом доступа к сообщениям новым приложениям, однако можно воспользоваться лазейкой и получить токен используя стороннее приложение. Например, [пройдя по этой ссылке](https://oauth.vk.com/authorize?client_id=2685278&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=messages,offline&response_type=token&v=5.52) вы сможете получить бессрочный токен от приложения Kate Mobile. После подтверждения авторизации токен появится в адресной строке.

Айди беседы должен быть ваш, не жабабота. Зайдите в беседу и посмотрите на адресную строку. Будет примерно так:

```
https://vk.com/im?sel=c281
                       └─┘
```

В данном случае айди беседы — 281.

Есть еще флаг `--debug`, с которым бот будет отправлять немного информации для дебага в беседу.

Если хочется скомпилить и запустить, то нужно установить SBT, затем запустить `sbt run`.

## Зачем я потратил на это время?

1. Было нечего делать
2. Практика в скале
3. Потому что мне не нравится эта игра и я хочу ее сломать 😡😡😡
