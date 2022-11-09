import 'package:christer/model/message.dart';

class UserChat{
  String img;
  String name;
  List<Message> messages;

  UserChat({required this.img, required this.name, required this.messages});
}