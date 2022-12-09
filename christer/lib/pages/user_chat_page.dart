import 'dart:async';

import 'package:christer/data/default_params.dart';
import 'package:christer/main.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
import 'package:christer/model/user_chat.dart';
import 'package:christer/theme/colors.dart';
import 'package:christer/model/message.dart';
import 'package:web_socket_channel/web_socket_channel.dart';

class UserChatPage extends StatefulWidget {
  UserChat userchat;

  UserChatPage({Key? key, required this.userchat}) : super(key: key);

  @override
  _UserChatPageState createState() => _UserChatPageState();
}

class _UserChatPageState extends State<UserChatPage> {
  final myController = TextEditingController();
  final _channel = WebSocketChannel.connect(Uri.parse(wsHost));
  late final StreamSubscription incoming;

  @override
  void initState() {
    super.initState();
  }

  @override
  void didChangeDependencies() {
    widget.userchat.messages.then((msgs) {
      incoming = _channel.stream.listen((msg) {
        setState(() {
          msgs.add(Message(isYours: false, msg: msg, created_at: "2:22 pm"));
        });
      });
    });
  }

  @override
  void dispose() {
    _channel.sink.close();
    myController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    var user = UserContext.of(context);
    ;
    return Scaffold(
        appBar: AppBar(
          title: Text(widget.userchat.name),
          backgroundColor: black,
        ),
        backgroundColor: white,
        body: FutureBuilder(
          future: widget.userchat.messages,
          builder: (context, snapshot) {
            if (snapshot.hasError) {
              return const Text('Could not load messages');
            } else if (!snapshot.hasData) {
              return const CircularProgressIndicator();
            }

            var messages = snapshot.requireData;
            return Column(
              children: [
                Expanded(
                  child: Padding(
                      padding: const EdgeInsets.symmetric(
                          horizontal: 20, vertical: 10),
                      child: ListView.builder(
                        itemCount: messages.length,
                        itemBuilder: (context, index) => Padding(
                          padding: const EdgeInsets.only(top: 20),
                          child: Row(
                              mainAxisAlignment: messages[index].isYours
                                  ? MainAxisAlignment.end
                                  : MainAxisAlignment.start,
                              children: [
                                Container(
                                  padding: const EdgeInsets.symmetric(
                                    horizontal: 15,
                                    vertical: 10,
                                  ),
                                  decoration: BoxDecoration(
                                    color: (messages[index].isYours
                                        ? primary
                                        : Colors.grey),
                                    borderRadius: BorderRadius.circular(30),
                                  ),
                                  child: Text(
                                    messages[index].msg,
                                    style: TextStyle(
                                      color: messages[index].isYours
                                          ? Colors.white
                                          : Theme.of(context)
                                              .textTheme
                                              .bodyText1!
                                              .color,
                                    ),
                                  ),
                                ),
                              ]),
                        ),
                      )),
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
                                  onPressed: () {
                                    var text = myController.text;

                                    if (text.isEmpty) {
                                      return;
                                    }

                                    _channel.sink.add(text);
                                    print("Message sent!");
                                    //insert to DB
                                    //...
                                    //local insert
                                    setState(() {
                                      messages.add(Message(
                                          isYours: true,
                                          msg: "${text}",
                                          created_at: "2:22 pm"));
                                      myController.text = "";
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
            );
          },
        ));
  }
}
