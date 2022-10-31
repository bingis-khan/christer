//import 'dart:html';

//import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:christer/theme/colors.dart';

class ChatPage extends StatefulWidget {
  @override
  _ChatPageState createState() => _ChatPageState();
}

class _ChatPageState extends State<ChatPage> {
  @override
  Widget build(BuildContext context){
    return Scaffold(
      backgroundColor: white,
      body: getBody(),
    );
  }

  Widget getBody() {
    return ListView(
      children: [
        Padding(
          padding: EdgeInsets.only(top: 20),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.spaceAround,
            children: [
              Text(
                "Messages",
                style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold, color: primary)
              ),
              Container(
                height: 25,
                width: 1,
                decoration: BoxDecoration(
                  color: black.withOpacity(0.15),
                ),
              ),
              Text(
                "Matches",
                style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold, color: black.withOpacity(0.5))
              ),
            ],
          ),
        ),
        SizedBox(
          height: 10,
        ),
        Divider(
          thickness: 0.8,
        ),
        Container(
          height: 38,
        ),
      ],
    );
  }
}