## Tips
### Vim Fold Markers
Needs `foldmethod=marker` on markdown buffers.

Sprint views wrap each top-level component in a `{{{1`/`}}}1` fold, so `zM` collapses the sprint down to its title lines and `zR` brings the contents back.

Closed issues/sub-issues wrap their content in vim fold markers using `{{{always` suffix (nested one level deeper inside a sprint).
To auto-close these folds in nvim, add:
```lua
vim.opt.foldtext = [[substitute(getline(v:foldstart),'{{{]] .. [[always\s*$','{{{','')]] -- Custom foldtext that strips "always" from fold markers
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function()
		vim.defer_fn(function()
			vim.cmd([[silent! g/{{]] .. [[{always$/normal! zc]])
		end, 10)
	end,
})
```
