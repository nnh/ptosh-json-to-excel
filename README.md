# JSON to Excel Converter

このプログラムはR言語で書かれており、R Studioでの実行を前提としています。入力としてJSONファイルを処理し、出力として.xlsxファイルを生成します。

## 使用手順
1. リポジトリの [Code] ボタンをクリックし、[Download ZIP] を選択してZIPファイルをダウンロードします。

2. ダウンロードしたZIPファイルを適当なディレクトリに「すべて展開」します。

1. R Studioを開きます。

2. R Studioのメニューから [File] > [New Project...] を選択します。

3. [New Project] ダイアログが表示されたら、[Existing Directory] を選択します。

4. [Project working directory] > [Browse...] を選択します。先ほど「すべて展開」したフォルダを選択し、[ptosh-json-to-excel-master] > [ptosh-json-to-excel-master]　を開いて[Open]　を選択します。

6. [Create Project] ボタンをクリックしてプロジェクトを開きます。

1. JSONファイルを `input` ディレクトリに格納してください。ファイルの拡張子は必ず `.json` である必要があります。 初期状態では「dummy」というファイルが存在しますが、あってもなくても処理には影響しません。

2. [File] > [Open File...]から、`prg/json_to_excel.R` ファイルを開いてください。
   
4. `source` をクリックし、 画面左下のconsoleに「処理が終了しました。」と出るまでお待ちください。

5. 処理されたJSONファイルに対応する.xlsxファイルが `output/output_処理年月/` ディレクトリに出力されます。初期状態では「dummy」というファイルが存在します。不要であれば削除して下さい。

6. 上記ディレクトリ内には、プログラムが生成した `list` フォルダがあり、その中には入力JSONファイルの内容を集計した `checklist.xlsx` が出力されます。

## ディレクトリ構造
```
ptosh-json-to-excel
├── LICENSE
├── README.md
├── input
│   ├── aaa.json
│   └── zzz.json
├── output
│   └── output_YYYYMMDDHHMMSS
│       ├── aaa.xlsx
│       ├── list
│       │   └── checklist.xlsx
│       └── zzz.xlsx
├── prg
│   ├── functions
│   └── json_to_excel.R
└── tools
```

## 注意事項

- プログラムが前提とするディレクトリ構造が整っていることを確認してください。
- 出力ディレクトリはプログラム中で自動的に作成されます。
