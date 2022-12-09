import 'dart:convert';

import 'package:christer/data/default_params.dart';
import 'package:christer/model/message.dart';
import 'package:christer/model/user_chat.dart';
import 'package:flutter/material.dart';
import 'package:christer/model/match.dart';
import 'package:http/http.dart' as http;
import 'user_context.dart';

Map<String, String> authHeaders(user) => {
      'Authorization':
          'Basic ${base64.encode(utf8.encode('${user.email}:${user.password}'))}'
    };

Future<List<Match>?> fetchMatches(User user) async {
  var res = await http.get(Uri.parse('$host/suggestions'),
      headers: authHeaders(user));

  if (res.statusCode != 200) {
    return null;
  }

  Iterable l = json.decode(res.body);
  return l.map((i) => Match.fromJson(i)).toList();
}

Future<bool> tryLogin(User user) async {
  var matches = await fetchMatches(user);

  return matches != null;
}

Future<List<String>> register(User user) async {
  var res = await http.post(Uri.parse('$host/register'),
      body: jsonEncode({'email': user.email, 'password': user.password}),
      headers: {'Content-Type': 'application/json'});

  if (res.statusCode != 200) {
    return ["Error during register."];
  } else {
    var dc = json.decode(res.body);

    if (dc == null) return [];

    List<String>? errors = List<String>.from(dc);
    return errors;
  }
}

Future<void> decide(User user, int id, bool decision) async {
  await http.put(
      Uri.parse('$host/$id/decide?verdict=${decision ? 'Yay' : 'Nay'}'),
      headers: authHeaders(user));
}

Future<Image> fetchImage(int userId) async {
  return Image.network('$host/pic/$userId');
}

Future<Image> fetchOwnImage(User user) async {
  return Image.network('$host/own-pic', headers: authHeaders(user)); // todo
}

Future<List<UserChatPresentation>> fetchDigest(User user) async {
  var res =
      await http.get(Uri.parse('$host/contacts'), headers: authHeaders(user));
  Iterable l = json.decode(res.body);
  return l.map((i) => UserChatPresentation.fromJson(i)).toList();
}

Future<List<Message>> fetchChat(User user, int otherID) async {
  return [];
}

Future<String?> uploadImage(User user, String path) async {
  var request = http.MultipartRequest("POST", Uri.parse('$host/post-pic'));
  request.headers.addAll(authHeaders(user));
  request.files.add(await http.MultipartFile.fromPath('file', path));

  return await request.send().then((response) {
    if (response.statusCode == 200) return null;
    return 'Error: ${response.statusCode}';
  });
}
