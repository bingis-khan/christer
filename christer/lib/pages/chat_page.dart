import 'package:christer/persist/persist.dart';
import 'package:christer/persist/user_context.dart';
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
  late Future<List<UserChatPresentation>> chats;
  String filter = '';

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();

    chats = fetchDigest(UserContext.of(context));
  }

  List<UserChatPresentation> filterChats(List<UserChatPresentation> chats) =>
      chats.where((c) => c.name.toLowerCase().contains(filter)).toList();

  void setFilter(String newFilter) {
    setState(() {
      filter = newFilter.toLowerCase();
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
    var user = UserContext.of(context);
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
              onChanged: setFilter,
            ),
          ),
        ),
        const SizedBox(
          height: 5,
        ),
        Padding(
            padding: const EdgeInsets.all(5),
            child: FutureBuilder(
              future: chats,
              builder: (context, snapshot) {
                if (snapshot.hasError) {
                  return const Text('error');
                } else if (!snapshot.hasData) {
                  return const CircularProgressIndicator();
                }

                var ufChats = snapshot.requireData;
                var chats = filterChats(ufChats);
                return Column(
                  children: chats.map((chat) {
                    return Padding(
                      padding: EdgeInsets.all(10),
                      child: ListTile(
                        onTap: () {
                          UserContext.push(
                              context,
                              UserChatPage(
                                  userchat: UserChat(
                                      id: chat.id,
                                      name: chat.name,
                                      messages: fetchChat(user, chat.id))));
                        },
                        visualDensity:
                            VisualDensity(horizontal: 0, vertical: -4),
                        contentPadding: EdgeInsets.only(
                            left: 0.0, right: 0.0, top: 10, bottom: 10),
                        leading: FutureBuilder(
                          future: chat.img,
                          builder: (context, snapshot) {
                            if (snapshot.hasError || !snapshot.hasData) {
                              return const CircularProgressIndicator();
                            }

                            var image = snapshot.requireData;
                            return CircleAvatar(
                              radius: 35,
                              backgroundImage: image.image,
                            );
                          },
                        ),
                        title: Text(
                          chat.name,
                          style: const TextStyle(
                            fontSize: 22,
                            color: white,
                            fontWeight: FontWeight.bold,
                          ),
                        ),
                        subtitle: Text(
                          chat.lastMessage != null
                              ? ((chat.lastMessage!.isYours ? 'You: ' : '') +
                                  (chat.lastMessage!.msg.length < MAX_LENGTH
                                      ? chat.lastMessage!.msg
                                      : chat.lastMessage!.msg
                                              .substring(0, MAX_LENGTH) +
                                          ' ... ') +
                                  ' - ' +
                                  chat.lastMessage!.created_at)
                              : "Say hi to your new match!",
                          style: const TextStyle(
                            fontSize: 15,
                            color: white,
                          ),
                        ),
                      ),
                    );
                  }).toList(),
                );
              },
            )),
      ],
    );
  }
}
