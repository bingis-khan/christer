import 'package:flutter/material.dart';
import 'package:christer/model/match.dart';

import 'user_context.dart';

// Basic stuff
int matches = 0;
Image defaultImage = Image.network(
    'https://cdn.cloudflare.steamstatic.com/steam/apps/364190/ss_7e0e4b0cb2cf6d266b9814e4d51e06cc06d0a91a.1920x1080.jpg?t=1572321559');
Future<List<Match>> fetchMatches(User user) async {
  const amount = 5;
  matches += amount;
  await Future.delayed(const Duration(milliseconds: 1000));
  return List.generate(amount, (i) => i + matches - amount)
      .map((i) => Match(
          id: i,
          firstName: 'bob$i',
          lastName: 'bobbi',
          age: i,
          height: i,
          race: '',
          description: '',
          image: defaultImage))
      .toList();
}

Future<void> decide(User user, int id, bool decision) async {}
