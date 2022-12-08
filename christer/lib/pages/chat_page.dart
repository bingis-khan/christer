import 'package:flutter/material.dart';
import 'package:christer/model/user_chat.dart';
import 'package:christer/theme/colors.dart';
import 'package:christer/data/chats_json.dart';
import 'package:christer/pages/user_chat_page.dart';

class ChatPage extends StatefulWidget {
  @override
  _ChatPageState createState() => _ChatPageState();
}

class _ChatPageState extends State<ChatPage> {
  final myController = TextEditingController();
  List<UserChat> chats = chats_json;

  void _updateChats(String name) {
    final filtered_chats = chats_json.where((element) {
      return element.name.toLowerCase().contains(name.toLowerCase());
    }).toList();
    setState(() {
      chats = filtered_chats;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: black,
      body: getBody(),
    );
  }

  @override
  void dispose() {
    myController.dispose();
    super.dispose();
  }

  Widget getBody() {
    var size = MediaQuery.of(context).size;
    const MAX_LENGTH = 20;
    return ListView(
      children: [
        Padding(
          padding: const EdgeInsets.all(8),
          child: Container(
            height: 48,
            decoration: BoxDecoration(
                color: white.withOpacity(0.5),
                borderRadius: BorderRadius.circular(24)),
            child: TextField(
              cursorColor: black.withOpacity(0.5),
              decoration: InputDecoration(
                  border: InputBorder.none,
                  prefixIcon: Icon(
                    Icons.search,
                    color: black.withOpacity(0.5),
                  ),
                  hintText: "Search"),
              controller: myController,
              onChanged: _updateChats,
            ),
          ),
        ),
        const SizedBox(
          height: 5,
        ),
        Padding(
          padding: const EdgeInsets.all(5),
          child: Column(
            children: List.generate(chats.length, (index) {
              return Padding(
                padding: EdgeInsets.all(10),
                child: ListTile(
                  onTap: () {
                    Navigator.push(
                        context,
                        MaterialPageRoute(
                            builder: (context) =>
                                UserChatPage(userchat: chats[index])));
                  },
                  visualDensity: VisualDensity(horizontal: 0, vertical: -4),
                  contentPadding: EdgeInsets.only(
                      left: 0.0, right: 0.0, top: 10, bottom: 10),
                  leading: CircleAvatar(
                    radius: 35,
                    backgroundImage: AssetImage(chats[index].img),
                  ),
                  title: Text(
                    chats[index].name,
                    style: const TextStyle(
                      fontSize: 22,
                      color: white,
                      fontWeight: FontWeight.bold,
                    ),
                  ),
                  subtitle: Text(
                    chats[index].messages.length > 0
                        ? ((chats[index]
                                    .messages[chats[index].messages.length - 1]
                                    .isYours
                                ? 'You: '
                                : '') +
                            (chats[index]
                                        .messages[
                                            chats[index].messages.length - 1]
                                        .msg
                                        .length <
                                    MAX_LENGTH
                                ? chats[index]
                                    .messages[chats[index].messages.length - 1]
                                    .msg
                                : chats[index]
                                        .messages[
                                            chats[index].messages.length - 1]
                                        .msg
                                        .substring(0, MAX_LENGTH) +
                                    ' ... ') +
                            ' - ' +
                            chats[index]
                                .messages[chats[index].messages.length - 1]
                                .created_at)
                        : "Say hi to your new match!",
                    style: const TextStyle(
                      fontSize: 15,
                      color: white,
                    ),
                  ),
                ),
              );
            }),
          ),
        ),
      ],
    );
  }
}
