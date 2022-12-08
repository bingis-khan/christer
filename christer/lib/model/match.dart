import 'package:christer/persist/persist.dart';
import 'package:flutter/material.dart';

class Match {
  final int id;
  final String firstName;
  final String lastName;
  final int age;
  final int height;
  final String race;
  final String description;
  final Future<Image> image;

  Match(
      {required this.id,
      required this.firstName,
      required this.lastName,
      required this.age,
      required this.height,
      required this.race,
      required this.description,
      required this.image});

  @override
  String toString() => firstName;

  factory Match.fromJson(Map<String, dynamic> json) => Match(
      id: json['id'],
      firstName: json['firstName'],
      lastName: json['lastName'],
      age: json['age'],
      height: json['height'],
      race: json['race'],
      description: json['description'],
      image: fetchImage(json['id']));
}
