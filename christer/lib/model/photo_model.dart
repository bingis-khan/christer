
import 'package:flutter/cupertino.dart';
import 'package:christer/persist/user_context.dart';

class PhotoModel extends ChangeNotifier{
  Image? _profile_image = null;

  Image? get getImage => _profile_image;

  void setImage(Image _image){
    _profile_image = _image;
    notifyListeners();
  }

  PhotoModel();
}