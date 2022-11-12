import 'package:flutter/material.dart';
import 'package:flutter/cupertino.dart';
import 'package:christer/model/user_chat.dart';
import 'package:christer/theme/colors.dart';
import 'package:christer/model/message.dart';

class UserChatPage extends StatefulWidget {
  UserChat userchat;

  UserChatPage({Key? key, required this.userchat}) : super(key: key);

  @override
  _UserChatPageState createState() => _UserChatPageState();
}

class _UserChatPageState extends State<UserChatPage> {
  final myController = TextEditingController();

  @override
  void dispose() {
    myController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context){
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.userchat.name),
        backgroundColor: black,
      ),
      backgroundColor: white,
      body: Column(
        children: [
          Expanded(
            child: Padding(
              padding: const EdgeInsets.symmetric(horizontal: 20, vertical: 10),
              child: ListView.builder(
                itemCount: widget.userchat.messages.length,
                itemBuilder: (context, index) =>
                    Padding(
                      padding: const EdgeInsets.only(top: 20),
                      child: Row(
                        mainAxisAlignment:
                            widget.userchat.messages[index].isYours ? MainAxisAlignment.end : MainAxisAlignment.start,
                        children: [
                          Container(
                            padding: const EdgeInsets.symmetric(
                              horizontal: 15,
                              vertical: 10,
                            ),
                            decoration: BoxDecoration(
                              color: (widget.userchat.messages[index].isYours ? primary : Colors.grey) ,
                              borderRadius: BorderRadius.circular(30),
                            ),
                            child: Text(
                              widget.userchat.messages[index].msg,
                              style: TextStyle(
                                color: widget.userchat.messages[index].isYours
                                    ? Colors.white
                                    : Theme.of(context).textTheme.bodyText1!.color,
                              ),
                            ),
                          ),
                        ],
                      ),
                    ),
              ),
            ),
          ),
          Container(
            padding: const EdgeInsets.symmetric(
              horizontal: 20,
              vertical: 10,
            ),
            decoration: BoxDecoration(
              color: Theme.of(context).scaffoldBackgroundColor,
              boxShadow: [
                BoxShadow(
                  offset: Offset(0, 4),
                  blurRadius: 32,
                  color: Color(0xFF087949).withOpacity(0.08),
                ),
              ],
            ),
            child: SafeArea(
              child: Row(
                children: [
                  Expanded(
                    child: Container(
                      padding: const EdgeInsets.symmetric(
                        horizontal: 20 * 0.75,
                      ),
                      decoration: BoxDecoration(
                        color: Color(0xFF00BF6D).withOpacity(0.05),
                        borderRadius: BorderRadius.circular(40),
                      ),
                      child: Row(
                        children: [
                          Expanded(
                            child: TextField(
                              controller: myController,
                              decoration: InputDecoration(
                                hintText: "Type message",
                                hintStyle: TextStyle(color: primary),
                                border: InputBorder.none,
                              ),
                            ),
                          ),
                          IconButton(
                            icon: const Icon(
                              Icons.send,
                              color: primary,
                            ),
                            tooltip: "Send",
                            onPressed: (){
                              print("Message sent!");
                              //insert to DB
                              //...
                              //local insert
                              setState((){
                                widget.userchat.messages.add(Message(isYours: true, msg: "${myController.text}", created_at: "2:22 pm"));
                              });
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
        ],
      ),
    );
  }
}