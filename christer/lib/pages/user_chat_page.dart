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
      backgroundColor: primary,
      body: Padding(
        padding: EdgeInsets.all(10),
        child: Container(
          height: 48,
          decoration: BoxDecoration(
              color: white.withOpacity(0.5),
              borderRadius: BorderRadius.circular(24)),
          child: SafeArea(
            child: Row(
              children: [
                Expanded(
                  child: Container(
                    padding: EdgeInsets.symmetric(
                      horizontal: 20.0 * 0.75,
                    ),
                    decoration: BoxDecoration(
                      color: Color(0xFF00BF6D).withOpacity(0.05),
                      borderRadius: BorderRadius.circular(40),
                    ),
                    child: Row(
                      children: [
                        Expanded(
                          child: TextField(
                            decoration: InputDecoration(
                              hintText: "Type message",
                              border: InputBorder.none,
                            ),
                          ),
                        ),
                        IconButton(
                          icon: Icon(
                            Icons.send,
                            color: Theme.of(context)
                                .textTheme
                                .bodyText1!
                                .color!
                                .withOpacity(0.64),
                          ),
                          tooltip: "Send",
                          onPressed: (){
                            print("Message sent!");
                          },
                        ),
                        
                      ],
                    ),
                  ),
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}