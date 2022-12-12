// ignore_for_file: unnecessary_brace_in_string_interps

import 'dart:convert';

import 'package:christer/data/default_params.dart';
import 'package:christer/pages/root_app.dart';
import 'package:christer/persist/persist.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
import 'package:flutter_login/flutter_login.dart';
import 'package:http/http.dart' as http;
import 'package:provider/provider.dart';
//import 'package:flutter_session/flutter_session.dart';

class LoginScreen extends StatefulWidget {
  const LoginScreen({super.key});

  @override
  State<LoginScreen> createState() => _LoginScreenState();
}

class _LoginScreenState extends State<LoginScreen> {
  late String email, password;

  Duration get loginTime => Duration(milliseconds: 2250);

  Future<String?> _authUser(LoginData data) async {
    debugPrint('Name: ${data.name}, Password: ${data.password}');
    return tryLogin(User(email: data.name, password: data.password))
        .then((succeeded) {
      if (!succeeded) {
        return "Invalid name or password";
      }

      setState(() {
        email = data.name;
        password = data.password;
      });

      return null;
    });
  }

  Future<String?> _signupUser(SignupData data) async {
    var errors = await register(
        User(email: data.name ?? '', password: data.password ?? ''));

    if (errors.isNotEmpty) {
      return errors.reduce((value, element) => '$value, $element');
    }

    setState(() {
      email = data.name ?? '';
      password = data.password ?? '';
    });

    return null;
  }

  Future<String> _recoverPassword(String name) {
    debugPrint('Name: $name');
    return Future<String>.value("Not supported");
  }

  @override
  Widget build(BuildContext context) {
    return FlutterLogin(
      title: 'CHRISTER',
      logo: const AssetImage('assets/images/girls/img_11.jpeg'),
      onLogin: _authUser,
      onSignup: _signupUser,
      onSubmitAnimationCompleted: () {
        context
            .read<UserContext>()
            .login(User(email: email, password: password));
      },
      onRecoverPassword: _recoverPassword,
    );
  }
}
