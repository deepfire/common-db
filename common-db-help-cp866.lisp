;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10 -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :comdb)


(defvar *comdb-help-ru*
  "~%��ଠ� ����᪠: ~A [���������...]

  ����ன�� ���-�।�:
    --disable-debugger          �⪫���� �⫠�稪 ��ᯠ.  �� �訡���
                                  ������ ���� �⥪� � ��室���.
    --break-on-signals          ��⠭���������� �� �᪫��⥫��� ������
                                  ������ ��ࠡ��稪.
    --early-break-on-signals    ������� ��⠭���� �� �᪫��⥫���
                                  ������ ࠭��.

  ����ன�� ��饣� �ࠪ��:
    --before-hook <lisp-expr>   �믮����� ���-��ࠦ���� ��। ᪠��஢�����
                                  ����䥩ᮢ.
    --core-multiplier <integer> ��⠭����� �����⥫� ����� ���⠫��.
    --no-scan                   �� ᪠��஢��� ����䥩��.
    --run-tests                 �믮����� �।���⥫�� ����.
                                  �� ������������� �訡�� ���.
    --ignore-test-failures      �����஢��� �訡�� �� �ᯮ������ ��⮢.
    --quit-after-tests          ��� ��᫥ �ᯮ������ ��⮢.
    --no-rc                     �� ����㦠�� ~~/.comdbrc
    --load <filename>           �ᯮ����� ������� �� 䠩��.
    --quit-after-load           ��� ��᫥ �ᯮ������ ������ �� 䠩��.
    --disable-usb               �� �ᯮ�짮���� USB-������� JTAG.
    --no-memory-detection       �� ������� ��।����� ⨯ �᭮���� �����.
    --memory-detection-threshold  �� ��।������ ⨯� �᭮���� �����
                                  ���஢��� ����訡�筮��� �����/�뢮��
                                  ⠪��� ���� �����.
    --verbose                   ������ ������ ���������� �� �६� ࠡ���.
    --help                      �뢥�� ��� �ࠢ��.
    --help-en                   �뢥�� �ࠢ��, �� ������᪮�.")

(defvar *flasher-help-ru*
  "  ����ன�� ����:
    --disable-debugger          �⪫���� �⫠�稪 ��ᯠ.  �� �訡���
    --raw-file <raw-filename>   ����㧨�� 䠩� ��� ����, ��� ࠧ���,
                                  �� ����� ��������� � ������� ����
                                  --raw-base.
    --raw-base <raw-filename-base>  ������� ���� ����㧪�, �ᯮ������
                                    ᮢ���⭮ � ����� --raw-file.
    --elf-file <ELF-filename>   ����㧨�� ᥪ樨 ���� � ������ �� ELF-䠩��.
    --preserve-holes            ���࠭��� ����� �ᥤ����騥 � 
                                  �����뢠��묨 �� ������ ��࠭��.
    --flash-base                ������� ������ ���� ���-�����.
                                  �� 㬮�砭�� ࠢ�� #xBFC00000.
    --dump-base <address>       ������ ᮤ�ন��� ����� 楫����� ���ன�⢠.
    --dump-size <bytes>         ������� ������⢮ ����� ��� ����.
                                  �� 㬮�砭�� ࠢ�� #x100.
    --no-check                  �� �஢����� 楫��⭮��� ����ᠭ��� ������.
    --dry-run                   ���㫨஢��� ������: ������ ��� �஬� ���.")