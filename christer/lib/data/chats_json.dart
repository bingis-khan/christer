// users story list
const List chats_json = [
  {
    "img": "assets/images/girls/img_1.jpeg",
    "name": "Ayo",
    "messages": [],
  },
  {
    "img": "assets/images/girls/img_2.jpeg",
    "name": "Rondeau",
    "messages": [
      {
        "isYours": true,
        "msg": "Hello, how are you? :)",
        "created_at": "1:12 pm"
      },
      {
        "isYours": true,
        "msg": "Hello?",
        "created_at": "3:45 pm"
      },
      {
        "isYours": false,
        "msg": "I got a boyfriend",
        "created_at": "3:50 pm"
      },
    ],
  },
  {
    "img": "assets/images/girls/img_3.jpeg",
    "name": "Valerie",
    "messages": [
      {
        "isYours": true,
        "msg": "Hi, how's your day? :)",
        "created_at": "2:13 pm"
      },
      {
        "isYours": false,
        "msg": "Omae wa mou shindeiru",
        "created_at": "2:45 pm"
      },
    ],
  },
  {
    "img": "assets/images/girls/img_4.jpeg",
    "name": "Mary",
    "messages": [],
  },
  {
    "img": "assets/images/girls/img_5.jpeg",
    "name": "Angie",
    "messages": [
      {
        "isYours": false,
        "msg": "DO NOT EVER DARE WRITE TO ME",
        "created_at": "1:12 pm"
      },
      {
        "isYours": true,
        "msg": "Ok, bye :(",
        "created_at": "1:15 pm"
      },
    ],
  },
  {
    "img": "assets/images/girls/img_6.jpeg",
    "name": "Anne",
    "messages": [],
  },
  {
    "img": "assets/images/girls/img_7.jpeg",
    "name": "Fineas",
    "messages": [],
  },
  {
    "img": "assets/images/girls/img_8.jpeg",
    "name": "Atikh",
    "messages": [],
  },
  {
    "img": "assets/images/girls/img_9.jpeg",
    "name": "Campbell",
    "messages": [],
  },
  {
    "img": "assets/images/girls/img_10.jpeg",
    "name": "Maya",
    "messages": [
      {
        "isYours": true,
        "msg": "Hello, how are you? :)",
        "created_at": "2:34 pm"
      },
      {
        "isYours": false,
        "msg": "[USER BLOCKED YOU]",
        "created_at": "2:35 pm"
      },
    ],
  },
];

// users message list
List userMessages = [
  {
    "id": 1,
    "name": "Ayo",
    "img": "assets/images/girls/img_1.jpeg",
    "online": true,
    "story": true,
    "message": "How are you doing?",
    "created_at": "1:00 pm"
  },
  {
    "name": "Rondeau",
    "img": "assets/images/girls/img_2.jpeg",
    "online": true,
    "story": false,
    "message": "Long time no see!!",
    "created_at": "12:00 am"
  },
  {
    "id": 3,
    "name": "Valerie",
    "img": "assets/images/girls/img_3.jpeg",
    "online": true,
    "story": true,
    "message": "Glad to know you in person!",
    "created_at": "3:30 pm"
  },
  {
    "id": 4,
    "name": "Anne",
    "img": "assets/images/girls/img_4.jpeg",
    "online": false,
    "story": false,
    "message": "I'm doing fine and how about you?",
    "created_at": "9:00 am"
  },
  {
    "id": 5,
    "name": "Fineas",
    "img": "assets/images/girls/img_5.jpeg",
    "online": true,
    "story": false,
    "message": "What is your real name?",
    "created_at": "11:25 am"
  },
  {
    "id": 6,
    "name": "Maya",
    "img": "assets/images/girls/img_6.jpeg",
    "online": false,
    "story": true,
    "message": "I'm happy to be your friend",
    "created_at": "10:00 am"
  },
];