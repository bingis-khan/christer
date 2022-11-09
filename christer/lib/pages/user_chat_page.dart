import 'package:flutter/material.dart';
import 'package:flutter/cupertino.dart';
import 'package:christer/model/user_chat.dart';
import 'package:christer/theme/colors.dart';

class UserChatPage extends StatefulWidget {
  final UserChat userchat;

  UserChatPage({Key? key, required this.userchat}) : super(key: key);

  @override
  _UserChatPageState createState() => _UserChatPageState();
}

class _UserChatPageState extends State<UserChatPage> {
  @override
  Widget build(BuildContext context){
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.userchat.name),
        backgroundColor: black,
      ),
      body: Text(widget.userchat.name),
    );
  }
}