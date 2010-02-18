;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: VIRTCORE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2010 by
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

(in-package :virtcore)


(bitmop:extend-namespace :core
  (:layouts
   ((:vpipeline "MPORT control registers.")
    (:fetch      0)
    (:dec        1)
    (:exec       2)
    (:mem        3)
    (:writeback  4)
    (:insn       5))))

(bitmop:extend-namespace :platform
  (:register-formats
   (:vcscon ""
     (:addrmask         8 0 "ChipSelect Mask: how large is the mapping")
     (:busaddr          8 8 "ChipSelect Bus Address, 31:24 of the physical bus address")
     (:wait-states      4 16 "Wait States for asynchronous memory accesses")
     (:enable           1 20 "Enable this ChipSelect")
     (:type             2 21 "Type: 00 async+wait, 10 async+nowait, 01 sync+dyn, 11 sync+static"
        ((#b00  :async+wait "")
         (#b01  :sync+dyn "")
         (#b10  :async+nowait "")
         (#b11  :sync+static ""))))
   (:vmemcon ""
     (:page        4 0 "Page Size"
        ((#b00  :128 "")
         (#b01  :256 "")
         (#b10  :512 "")
         (#b11  :1024 "")))
     (:refresh          10 4 "Refresh Cycle Count")
     (:cas-latency      1 20 "CAS Latency"
        ((#b0  :cl-2 "")
         (#b1  :cl-3 "")))
     (:init             1 31 "Init"))
   (:vmemcon-d02 ""
     (:prefetch        10 4)
     (:page             4 0 "Page Size"
        ((#b00  :512 "")
         (#b01  :1024 "")
         (#b10  :2048 "")
         (#b11  :4096 "")))
     (:refresh         10 4 "Refresh Cycle Count")
     (:cas-latency      1 20 "CAS Latency"
        ((#b0  :cl-2 "")
         (#b1  :cl-3 "")))
     (:init             1 31 "Init")))
  (:layouts
   ((:vmport "MPORT control registers.")
    (:vcscon0        0      :format :vcscon)
    (:vcscon1        1      :format :vcscon)
    (:vmemcon        2      :format :vmemcon))
   ((:vmport-02 "MPORT control registers: v02.")
    (:vcscon0        0      :format :vcscon)
    (:vcscon1        1      :format :vcscon)
    (:vmemcon-d02    2      :format :vmemcon-d02))
   ((:vdma "DMA channel" :force-multi t)
    (:vdma.csr       0      :doc "Control/Status")
    (:vdma.base      1      :doc "Memory: transfer base address.")
    (:vdma.count     2      :doc "Memory: transfer word count."))
   ((:vport "Port" :force-multi t)
    (:vport.csr      0      :doc "Control/Status")
    (:vport.trx      1      :doc "IO buffer register")
    (:vport.ios      2      :doc "IO status register"))
   ((:vtimer "Interval timer")
    (:vtimer.csr     0      :doc "Control/Status")
    (:vtimer.period  1)
    (:vtimer.count   2))))