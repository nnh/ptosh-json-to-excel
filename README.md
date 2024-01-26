# JSON to Excel Converter

このプログラムはR言語で書かれており、R Studioでの実行を前提としています。入力としてJSONファイルを処理し、出力として.xlsxファイルを生成します。

## 使用手順

1. **入力ファイルの準備:**
   - JSONファイルを `prg/input/` ディレクトリに格納してください。
   - ファイルの拡張子は必ず `.json` である必要があります。

2. **プログラムの実行:**
   - R Studioを開き、`prg/json_to_excel.R` ファイルを開いてください。
   - `source` をクリックして実行してください。

3. **出力結果の確認:**
   - 処理されたJSONファイルに対応する.xlsxファイルが `prg/output/output_処理年月/` ディレクトリに出力されます。
   - 同ディレクトリ内には、プログラムが生成した `list` フォルダがあり、その中には入力JSONファイルの内容を集計した `checklist.xlsx` が出力されます。

## ディレクトリ構造
```
prg
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
