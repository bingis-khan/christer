import 'package:christer/model/message.dart';
import 'package:flutter/material.dart';

class UserChat {
  final int id;
  final String name;
  final Future<List<Message>> messages;

  UserChat({required this.id, required this.name, required this.messages});
}

class UserChatPresentation {
  final int id;
  final String name;
  final Message? lastMessage;
  final Future<Image> img;

  UserChatPresentation(
      {required this.id,
      required this.name,
      required this.lastMessage,
      required this.img});
}
