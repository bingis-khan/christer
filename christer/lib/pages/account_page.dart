import 'package:christer/data/account_json.dart';
import 'package:christer/pages/edit_page.dart';
import 'package:christer/pages/settings_page.dart';
import 'package:christer/pages/image_picker.dart';
import 'package:christer/persist/user_context.dart';
import 'package:christer/theme/colors.dart';
import 'package:flutter/material.dart';
import 'package:flutter_custom_clippers/flutter_custom_clippers.dart';

class AccountPage extends StatefulWidget {
  @override
  _AccountPageState createState() => _AccountPageState();
}

class _AccountPageState extends State<AccountPage> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: grey.withOpacity(0.2),
      body: getBody(),
    );
  }

  Widget getBody() {
    var size = MediaQuery.of(context).size;
    var user = UserContext.of(context);
    return ClipPath(
      clipper: OvalBottomBorderClipper(),
      child: Container(
        width: size.width,
        height: size.height * 0.7,
        decoration: BoxDecoration(color: white, boxShadow: [
          BoxShadow(
              color: grey.withOpacity(0.1), spreadRadius: 10, blurRadius: 10)
        ]),
        child: Padding(
          padding: const EdgeInsets.only(left: 30, right: 30, bottom: 40),
          child: Column(mainAxisAlignment: MainAxisAlignment.end, children: [
            Container(
              width: 140,
              height: 140,
              decoration: BoxDecoration(
                  shape: BoxShape.circle,
                  image: DecorationImage(
                      image: AssetImage(account_json[0]['img']),
                      fit: BoxFit.cover)),
            ),
            SizedBox(
              height: 15,
            ),
            Text(
              user.email + ", " + user.password,
              style: TextStyle(fontSize: 25, fontWeight: FontWeight.w600),
            ),
            SizedBox(
              height: 20,
            ),
            Row(
              crossAxisAlignment: CrossAxisAlignment.start,
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Column(
                  children: [
                    Container(
                      width: 60,
                      height: 60,
                      decoration: BoxDecoration(
                          color: white,
                          shape: BoxShape.circle,
                          boxShadow: [
                            BoxShadow(
                              color: grey.withOpacity(0.1),
                              blurRadius: 15,
                              spreadRadius: 10,
                            )
                          ]),
                      child: IconButton(
                        color: grey.withOpacity(0.5),
                        icon: Icon(Icons.settings),
                        iconSize: 35,
                        onPressed: () {
                          UserContext.push(
                            context,
                            const SettingsPage(),
                          );
                        },
                      ),
                    ),
                    SizedBox(
                      height: 10,
                    ),
                    Text(
                      "SETTINGS",
                      style: TextStyle(
                          fontSize: 12,
                          fontWeight: FontWeight.w600,
                          color: grey.withOpacity(0.8)),
                    )
                  ],
                ),
                Padding(
                  padding: const EdgeInsets.only(top: 20),
                  child: Column(
                    children: [
                      Container(
                        width: 85,
                        height: 85,
                        child: Stack(
                          children: [
                            Container(
                              width: 80,
                              height: 80,
                              decoration: BoxDecoration(
                                  gradient: LinearGradient(
                                      colors: [primary_one, primary_two]),
                                  shape: BoxShape.circle,
                                  boxShadow: [
                                    BoxShadow(
                                      color: grey.withOpacity(0.1),
                                      blurRadius: 15,
                                      spreadRadius: 10,
                                    )
                                  ]),
                              child: IconButton(
                                color: white,
                                icon: Icon(Icons.camera_alt),
                                iconSize: 45,
                                onPressed: () {
                                  UserContext.push(
                                    context,
                                    PickPhotoScreen(),
                                  );
                                },
                              ),
                            ),
                            Positioned(
                              right: 8,
                              bottom: 8,
                              child: Container(
                                width: 25,
                                height: 25,
                                decoration: BoxDecoration(
                                    color: white,
                                    shape: BoxShape.circle,
                                    boxShadow: [
                                      BoxShadow(
                                        color: grey.withOpacity(0.1),
                                        blurRadius: 15,
                                        spreadRadius: 10,
                                      )
                                    ]),
                                child: Center(
                                  child: Icon(
                                    Icons.add,
                                    color: primary,
                                  ),
                                ),
                              ),
                            )
                          ],
                        ),
                      ),
                      SizedBox(
                        height: 10,
                      ),
                      Text(
                        "ADD MEDIA",
                        style: TextStyle(
                            fontSize: 12,
                            fontWeight: FontWeight.w600,
                            color: grey.withOpacity(0.8)),
                      )
                    ],
                  ),
                ),
                Column(
                  children: [
                    Container(
                      width: 60,
                      height: 60,
                      decoration: BoxDecoration(
                          color: white,
                          shape: BoxShape.circle,
                          boxShadow: [
                            BoxShadow(
                              color: grey.withOpacity(0.1),
                              blurRadius: 15,
                              spreadRadius: 10,
                            )
                          ]),
                      child: IconButton(
                        color: grey.withOpacity(0.5),
                        icon: Icon(Icons.edit),
                        iconSize: 35,
                        onPressed: () {
                          UserContext.push(context, const EditPage());
                        },
                      ),
                    ),
                    SizedBox(
                      height: 10,
                    ),
                    Text(
                      "EDIT INFO",
                      style: TextStyle(
                          fontSize: 12,
                          fontWeight: FontWeight.w600,
                          color: grey.withOpacity(0.8)),
                    )
                  ],
                ),
              ],
            ),
            SizedBox(height: 25,),
            CustomButton(
              title: 'Log out',
              icon: Icons.logout,
              onClick: () => {}
            ),
          ]),
        ),
      ),
    );
  }

  Widget CustomButton(
      {required String title,
      required IconData icon,
      required VoidCallback onClick}) {
    return Container(
      width: 200,
      child: ElevatedButton(
        onPressed: onClick,
        child: Row(
          children: [
            Icon(icon),
            SizedBox(width: 20),
            Text(title),
          ],
        ),
      ),
    );
  }
}
