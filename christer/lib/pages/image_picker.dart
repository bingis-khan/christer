import 'dart:convert';
import 'dart:typed_data';

import 'dart:io';
import 'package:christer/model/photo_model.dart';
import 'package:christer/persist/persist.dart';
import 'package:christer/persist/user_context.dart';
import 'package:flutter/material.dart';
//import 'package:flutter_svg/svg.dart';
import 'package:christer/theme/colors.dart';
//import 'package:image_picker/image_picker.dart';
//import 'package:path_provider/path_provider.dart';
import 'package:file_picker/file_picker.dart';

Future<Image> convertFileToImage(File picture) async {
  List<int> imageBase64 = picture.readAsBytesSync();
  String imageAsString = base64Encode(imageBase64);
  Uint8List uint8list = base64.decode(imageAsString);
  Image image = Image.memory(uint8list);
  return image;
}

class PickPhotoScreen extends StatefulWidget {
  final PhotoModel photoModel;

  PickPhotoScreen({Key? key, required this.photoModel}) : super(key: key);

  @override
  _PickPhotoScreenState createState() => _PickPhotoScreenState();
}

const errorSnackBar = SnackBar(content: Text('Too many or no files selected.'));

class _PickPhotoScreenState extends State<PickPhotoScreen> {
  late Future<Image> _image;

  Future getImage() async {
    FilePickerResult? result = await FilePicker.platform.pickFiles(
      type: FileType.custom,
      allowedExtensions: ['jpg', 'png', 'jpeg'],
    );

    // Check if the user has exited already (we don't care then).
    if (!mounted) return;

    if (result == null) return;

    if (!result.isSinglePick) {
      ScaffoldMessenger.of(context).showSnackBar(errorSnackBar);
      return;
    }

    var path = result.files.single.path!;
    var user = UserContext.of(context);
    var error = await uploadImage(user, path);

    var file = File(path);

    Image gotImage = await convertFileToImage(file);

    widget.photoModel.setImage(gotImage);

    if (error == null) {
      setState(() {
        _image = fetchOwnImage(user);
        widget.photoModel.setImage(gotImage);
      });
    }
  }

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();

    var user = UserContext.of(context);
    _image = fetchOwnImage(user);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: white,
      appBar: AppBar(
        title: Text("Pick a Photo"),
        backgroundColor: black,
      ),
      body: Center(
          child: Column(
        children: [
          SizedBox(height: 10),
          FutureBuilder(
            future: _image,
            builder: (context, snapshot) {
              if (!snapshot.hasData) {
                return const CircularProgressIndicator();
              }

              var image = widget.photoModel.getImage == null ? snapshot.requireData : widget.photoModel.getImage!;
              var size = MediaQuery.of(context).size;
              return Container(
                width: size.width / 2,
                height: size.height / 2,
                decoration: BoxDecoration(
                    image:
                        DecorationImage(image: image.image, fit: BoxFit.cover)),
              );
            },
          ),
          SizedBox(height: 40),
          CustomButton(
            title: 'Pick from Gallery',
            icon: Icons.image_outlined,
            onClick: getImage,
          ),
        ],
      )),
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
