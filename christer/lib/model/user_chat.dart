import 'package:christer/model/message.dart';
import 'package:christer/persist/persist.dart';
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
  final String? lastMessage;
  final Future<Image> img;

  UserChatPresentation(
      {required this.id,
      required this.name,
      required this.lastMessage,
      required this.img});

  factory UserChatPresentation.fromJson(Map<String, dynamic> json) =>
      UserChatPresentation(
          id: json['sender'],
          name: json['senderData']?['firstName'] ?? 'Anonymous',
          lastMessage: json['lastMessage'],
          img: fetchImage(json['sender']));
}
