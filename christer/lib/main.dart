import 'package:christer/pages/login.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
import 'package:christer/pages/root_app.dart';

void main() {
  runApp(MaterialApp(
    home: LoginScreen(),
    debugShowCheckedModeBanner: false,
  ));
}
