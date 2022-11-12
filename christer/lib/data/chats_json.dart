// users story list
import 'package:christer/model/message.dart';
import 'package:christer/model/user_chat.dart';

List<UserChat> chats_json = [
  UserChat(
    img: "assets/images/girls/img_1.jpeg",
    name: "Ayo",
    messages: [],
  ),
  UserChat(
    img: "assets/images/girls/img_2.jpeg",
    name: "Rondeau",
    messages: [
      Message(
        isYours: true,
        msg: "Hello, how are you? :)",
        created_at: "1:12 pm",
      ),
      Message(
        isYours: true,
        msg: "Hello?",
        created_at: "3:45 pm",
      ),
      Message(
        isYours: false,
        msg: "I got a boyfriend.",
        created_at: "4:57 pm",
      ),
    ],
  ),
  UserChat(
    img: "assets/images/girls/img_3.jpeg",
    name: "Valerie",
    messages: [
      Message(isYours: true, msg: "Hi, how's your day? :)", created_at: "2:13 pm"),
      Message(isYours: false, msg: "Omae wa mou shindeiru", created_at: "2:45 pm"),
      Message(isYours: true, msg: "Hi, how's your day? :)", created_at: "2:13 pm"),
      Message(isYours: false, msg: "Omae wa mou shindeiru2", created_at: "2:45 pm"),
      Message(isYours: true, msg: "Hi, how's your day? :)", created_at: "2:13 pm"),
      Message(isYours: false, msg: "Omae wa mou shindeiru3", created_at: "2:45 pm"),
      Message(isYours: true, msg: "Hi, how's your day? :)", created_at: "2:13 pm"),
      Message(isYours: false, msg: "Omae wa mou shindeiru4", created_at: "2:45 pm"),
      Message(isYours: true, msg: "Hi, how's your day? :)", created_at: "2:13 pm"),
      Message(isYours: false, msg: "Omae wa mou shindeiru5", created_at: "2:45 pm"),
      Message(isYours: true, msg: "Hi, how's your day? :)", created_at: "2:13 pm"),
      Message(isYours: false, msg: "Omae wa mou shindeiru6", created_at: "2:45 pm"),
    ],
  ),
  UserChat(
    img: "assets/images/girls/img_4.jpeg",
    name: "Mary",
    messages: [],
  ),
  UserChat(
    img: "assets/images/girls/img_5.jpeg",
    name: "Angie",
    messages: [
      Message(isYours: false, msg: "DO NOT EVER DARE WRITE TO ME", created_at: "1:12 pm"),
      Message(isYours: true, msg: "Ok, bye :(", created_at: "1:15 pm"),
    ],
  ),
  UserChat(
    img: "assets/images/girls/img_6.jpeg",
    name: "Anne",
    messages: [],
  ),
  UserChat(
    img: "assets/images/girls/img_7.jpeg",
    name: "Fineas",
    messages: [],
  ),
  UserChat(
    img: "assets/images/girls/img_8.jpeg",
    name: "Atikh",
    messages: [],
  ),
  UserChat(
    img: "assets/images/girls/img_9.jpeg",
    name: "Campbell",
    messages: [],
  ),
  UserChat(
    img: "assets/images/girls/img_10.jpeg",
    name: "Maya", 
    messages: [
      Message(isYours: true, msg: "Hello, how are you? :)", created_at: "2:34 pm"),
      Message(isYours: false, msg: "[USER BLOCKED YOU]", created_at: "2:35 pm"),
    ],
  ),
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